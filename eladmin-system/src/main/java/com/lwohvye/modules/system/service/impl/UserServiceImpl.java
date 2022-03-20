/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.modules.system.service.impl;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.RandomUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.core.util.StrUtil;
import com.lwohvye.config.FileProperties;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.exception.EntityExistException;
import com.lwohvye.exception.EntityNotFoundException;
import com.lwohvye.modules.security.service.UserLocalCache;
import com.lwohvye.modules.system.domain.Dept;
import com.lwohvye.modules.system.domain.User;
import com.lwohvye.modules.system.domain.projection.UserProj;
import com.lwohvye.modules.system.observer.DeptObserver;
import com.lwohvye.modules.system.observer.MenuObserver;
import com.lwohvye.modules.system.observer.RoleObserver;
import com.lwohvye.modules.system.repository.UserRepository;
import com.lwohvye.modules.system.service.IUserService;
import com.lwohvye.modules.system.service.dto.*;
import com.lwohvye.modules.system.service.mapstruct.UserInnerMapper;
import com.lwohvye.modules.system.service.mapstruct.UserMapper;
import com.lwohvye.modules.system.subject.UserSubject;
import com.lwohvye.utils.*;
import com.lwohvye.utils.redis.RedisUtils;
import io.jsonwebtoken.lang.Assert;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotBlank;
import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Service
@RequiredArgsConstructor
//配置该类缓存的公共前缀
@CacheConfig(cacheNames = "user")
public class UserServiceImpl extends UserSubject implements IUserService, RoleObserver, MenuObserver, DeptObserver {

    private final UserRepository userRepository;
    private final UserMapper userMapper;
    private final UserInnerMapper userInnerMapper;
    private final FileProperties properties;
    private final RedisUtils redisUtils;
    private final UserLocalCache userLocalCache;

    @PostConstruct
    @Override
    public void doInit() {
        SpringContextHolder.addCallBacks(this::doRegister);
    }

    /**
     * 注册观察者
     *
     * @date 2022/3/13 9:40 PM
     */
    @Override
    public void doRegister() {
        var roleService = SpringContextHolder.getBean("roleServiceImpl");
        ReflectUtil.invoke(roleService, "addObserver", this);

        var menuService = SpringContextHolder.getBean("menuServiceImpl");
        ReflectUtil.invoke(menuService, "addObserver", this);

        var deptService = SpringContextHolder.getBean("deptServiceImpl");
        ReflectUtil.invoke(deptService, "addObserver", this);

    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public Object queryAll(UserQueryCriteria criteria, Pageable pageable) {
        var page = userRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtil.toPage(page.map(user -> userMapper.toDto(user, new CycleAvoidingMappingContext()))); // 这里使用toPage，在无符合条件的记录时，也有构筑结果并缓存，也算是一定程度上缓解缓存穿透
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<UserDto> queryAll(UserQueryCriteria criteria) {
        List<User> users = userRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder));
        return userMapper.toDto(users, new CycleAvoidingMappingContext());
    }

    @Override
    public Object queryAll(User expUser, Pageable pageable) {
        var matcher = ExampleMatcher.matching()
                .withMatcher("username", ExampleMatcher.GenericPropertyMatcher::contains)
                .withMatcher("nickName", match -> match.startsWith().ignoreCase());
        var example = Example.of(expUser, matcher);
        return userRepository.findAll(example);
    }

    @Override
    @Cacheable(key = " #root.target.getSysName() + 'id:' + #p0")
    @Transactional(rollbackFor = Exception.class)
    public UserDto findById(long id) {
        User user = userRepository.findById(id).orElseGet(User::new);
        ValidationUtil.isNull(user.getId(), "User", "id", id);
        return userMapper.toDto(user, new CycleAvoidingMappingContext());
    }

    @Override
//  先简单处理，清理该域中所有缓存
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(User resources) {
        if (userRepository.findByUsername(resources.getUsername()) != null) {
            throw new EntityExistException(User.class, "username", resources.getUsername());
        }
        if (userRepository.findByEmail(resources.getEmail()) != null) {
            throw new EntityExistException(User.class, "email", resources.getEmail());
        }
        if (userRepository.findByPhone(resources.getPhone()) != null) {
            throw new EntityExistException(User.class, "phone", resources.getPhone());
        }
        userRepository.save(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void update(User resources) throws Exception {
        User user = userRepository.findById(resources.getId()).orElseGet(User::new);
        ValidationUtil.isNull(user.getId(), "User", "id", resources.getId());
        User user1 = userRepository.findByUsername(resources.getUsername());
        User user2 = userRepository.findByEmail(resources.getEmail());
        User user3 = userRepository.findByPhone(resources.getPhone());
        if (user1 != null && !user.getId().equals(user1.getId())) {
            throw new EntityExistException(User.class, "username", resources.getUsername());
        }
        if (user2 != null && !user.getId().equals(user2.getId())) {
            throw new EntityExistException(User.class, "email", resources.getEmail());
        }
        if (user3 != null && !user.getId().equals(user3.getId())) {
            throw new EntityExistException(User.class, "phone", resources.getPhone());
        }

//        var convertString4BlobUtil = new ConvertString4BlobUtil<User>();
//        不确定是否需要进行赋值。理论上传递的是引用。更改会影响到这方
//        convertString4BlobUtil.convert(user);

        user.setUsername(resources.getUsername());
        user.setEmail(resources.getEmail());
        user.setEnabled(resources.getEnabled());
        user.setRoles(resources.getRoles());
        user.setDept(resources.getDept());
        user.setJobs(resources.getJobs());
        user.setPhone(resources.getPhone());
        user.setNickName(resources.getNickName());
        user.setGender(resources.getGender());
        var description = resources.getDescription();
        if (StrUtil.isNotEmpty(description))
            user.setDescription(description);
        userRepository.save(user);

        // 发布用户更新事件
        notifyObserver(resources.getId());
        // 清除本地缓存
        flushCache(user.getUsername());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void updateCenter(User resources) {
        User user = userRepository.findById(resources.getId()).orElseGet(User::new);
        User user1 = userRepository.findByPhone(resources.getPhone());
        if (user1 != null && !user.getId().equals(user1.getId())) {
            throw new EntityExistException(User.class, "phone", resources.getPhone());
        }
        user.setNickName(resources.getNickName());
        user.setPhone(resources.getPhone());
        user.setGender(resources.getGender());
        userRepository.save(user);
    }

    @Override
    public int countByRoles(Collection<Long> rids) {
        return userRepository.countByRoles(rids);
    }

    @Override
    public int countByJobs(Collection<Long> jids) {
        return userRepository.countByJobs(jids);
    }

    @Override
    public Boolean hasDepts(List<Dept> depts) {
        return userRepository.existsByDeptIn(depts);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void delete(Set<Long> ids) {
        userRepository.deleteAllByIdIn(ids);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @Cacheable(key = " #root.target.getSysName() + 'userInfo:' + #p0")
    public UserDto findByName(String userName) {
        User user = userRepository.findByUsername(userName);
        // 这里只做标记用，当前业务暂不需要Projection
        if (RandomUtil.randomBoolean()) {
            var upj = userRepository.findByUsername(userName, UserProj.class);
        }
        if (Objects.isNull(user))
            throw new EntityNotFoundException(User.class, "name", userName);
        else
            return userMapper.toDto(user, new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public UserInnerDto findInnerUserByName(String userName) {
        // 方法内调用，Spring aop不会生效，所以若直接调 findByName(String username) 方法不会走缓存
        var user = userRepository.findByUsername(userName);
        if (Objects.isNull(user))
            throw new EntityNotFoundException(User.class, "name", userName);
        else
            return userInnerMapper.toDto(user, new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void updatePass(String username, String pass) {
        userRepository.updatePass(username, pass, new Date());
        flushCache(username);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public Map<String, String> updateAvatar(MultipartFile multipartFile) {
        // 文件大小验证
        FileUtil.checkSize(properties.getAvatarMaxSize(), multipartFile.getSize());
        // 验证文件上传的格式
        String image = "gif jpg png jpeg";
        String fileType = FileUtil.getExtensionName(multipartFile.getOriginalFilename());
        if (ObjectUtil.isNotNull(fileType) && !image.contains(fileType)) {
            throw new BadRequestException("文件格式错误！, 仅支持 " + image + " 格式");
        }
        User user = userRepository.findByUsername(SecurityUtils.getCurrentUsername());
        String oldPath = user.getAvatarPath();
        File file = FileUtil.upload(multipartFile, properties.getOSPath().getAvatar());
        user.setAvatarPath(Objects.requireNonNull(file).getPath());
        user.setAvatarName(file.getName());
        userRepository.save(user);
        if (StringUtils.isNotBlank(oldPath)) {
            FileUtil.del(oldPath);
        }
        @NotBlank String username = user.getUsername();
        flushCache(username);
        return Map.of("avatar", file.getName());

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void updateEmail(String username, String email) {
        userRepository.updateEmail(username, email);
        flushCache(username);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void updateEnabled(String username, Boolean enabled) {
        Assert.hasText(username, "用户名不可为空");
        userRepository.updateEnabled(username, enabled);
//        状态更新后，需清除相关信息
        flushCache(username);
    }

    @Override
    public void download(List<UserDto> queryAll, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (UserDto userDTO : queryAll) {
            List<String> roles = userDTO.getRoles().stream().map(RoleSmallDto::getName).toList();
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("用户名", userDTO.getUsername());
            map.put("角色", roles);
            map.put("部门", userDTO.getDept().getName());
            map.put("岗位", userDTO.getJobs().stream().map(JobSmallDto::getName).toList());
            map.put("邮箱", userDTO.getEmail());
            map.put("状态", Boolean.TRUE.equals(userDTO.getEnabled()) ? "启用" : "禁用");
            map.put("手机号码", userDTO.getPhone());
            map.put("修改密码的时间", userDTO.getPwdResetTime());
            map.put("创建日期", userDTO.getCreateTime());
            list.add(map);
        }
        FileUtil.downloadExcel(list, response);
    }

    /**
     * 清理 登陆时 用户缓存信息
     *
     * @param username /
     */
    private void flushCache(String username) {
        userLocalCache.cleanUserCache(username, true);
    }

    @Override
    public void roleUpdate(Object obj) {
        userRepository.findByRoleId((long) obj).forEach(user -> {
            userLocalCache.cleanUserCache(user.getUsername(), true);
            redisUtils.delInRC(CacheKey.USER_ID, user.getId());
            notifyObserver(user.getId());
        });
    }

    @Override
    public void menuUpdate(Object obj) {
        userRepository.findByMenuId((long) obj).forEach(user -> {
            userLocalCache.cleanUserCache(user.getUsername(), true);
            redisUtils.delInRC(CacheKey.USER_ID, user.getId());
            notifyObserver(user.getId());
        });
    }

    @Override
    public void deptUpdate(Object obj) {
        userRepository.findByRoleDeptId((long) obj).forEach(user -> {
            userLocalCache.cleanUserCache(user.getUsername(), true);
            redisUtils.delInRC(CacheKey.USER_ID, user.getId());
            notifyObserver(user.getId());
        });
    }
}
