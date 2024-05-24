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
package com.lwohvye.sys.modules.system.service.impl;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.RandomUtil;
import cn.hutool.core.util.StrUtil;
import com.lwohvye.api.modules.system.domain.Dept;
import com.lwohvye.api.modules.system.domain.User;
import com.lwohvye.api.modules.system.domain.projection.UserProj;
import com.lwohvye.api.modules.system.service.dto.*;
import com.lwohvye.sys.common.constant.SysCacheKey;
import com.lwohvye.beans.config.FileProperties;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.*;
import com.lwohvye.core.utils.redis.RedisUtils;
import com.lwohvye.sys.modules.security.service.UserLocalCache;
import com.lwohvye.sys.modules.system.event.DeptEvent;
import com.lwohvye.sys.modules.system.event.MenuEvent;
import com.lwohvye.sys.modules.system.event.RoleEvent;
import com.lwohvye.sys.modules.system.event.UserEvent;
import com.lwohvye.sys.modules.system.repository.UserRepository;
import com.lwohvye.sys.modules.system.service.IUserService;
import lombok.RequiredArgsConstructor;
import org.jetbrains.annotations.NotNull;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;
import org.springframework.context.event.EventListener;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.web.multipart.MultipartFile;

import jakarta.persistence.EntityExistsException;
import jakarta.persistence.EntityNotFoundException;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotBlank;

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
public class UserServiceImpl implements IUserService, ApplicationEventPublisherAware {

    private final UserRepository userRepository;

    private final ConversionService conversionService;
    private final FileProperties properties;
    private final RedisUtils redisUtils;
    private final UserLocalCache userLocalCache;

    private ApplicationEventPublisher eventPublisher;

    /*
    @PostConstruct
    @Override
    public void doInit() {
        SpringContextHolder.addCallBacks(this::doRegister);
    }

    */

    /**
     * 注册观察者
     *
     * @date 2022/3/13 9:40 PM
     */
    /*
    @Override
    public void doRegister() {
        var lookup = MethodHandles.lookup();
        Arrays.stream(this.getClass().getInterfaces())
                .filter(aClass -> aClass.getSimpleName().endsWith("Observer"))
                .forEach(aClass -> {
                    var aName = aClass.getSimpleName();
                    var aType = StringUtils.lowerFirstChar(aName.substring(0, aName.indexOf("Observer"))); // 首字母要转小写
                    var aService = SpringContextHolder.getBean(aType + "ServiceImpl");
                    var methodType = MethodType.methodType(void.class, new Class[]{aClass}); // 注意这里要用xxxObserver而不是this。获取时，methodType需与方法签名一致，不支持向上转型。返回值也不支持向上/向下转型（不支持向下很容易理解）
                    try {
                        // 这里在控制台 invoke 是不行的：MethodHandle.invoke cannot be invoked reflectively。另外这里注入的是未被代理的类，需注意一下
                        lookup.findVirtual(aService.getClass(), "addObserver", methodType).invoke(aService, this);
                    } catch (Throwable ignored) {
                    }
                    // ReflectUtil.invoke(aService, "addObserver", this);
                });
    }*/
    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(UserQueryCriteria criteria, Pageable pageable) {
        var page = userRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(user -> conversionService.convert(user, UserDto.class))); // 这里使用toPage，在无符合条件的记录时，也有构筑结果并缓存，也算是一定程度上缓解缓存穿透
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<UserDto> queryAll(UserQueryCriteria criteria) {
        return userRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder))
                .stream().map(user -> conversionService.convert(user, UserDto.class)).toList();
    }

    @Override
    public List<User> queryAll(User expUser, Pageable pageable) {
        var matcher = ExampleMatcher.matching()
                .withMatcher("username", ExampleMatcher.GenericPropertyMatcher::contains)
                .withMatcher("nickName", match -> match.startsWith().ignoreCase());
        var example = Example.of(expUser, matcher);
        return userRepository.findAll(example);
    }

    @Override
    @Cacheable(key = "'id:' + #p0")
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public UserDto findById(long id) {
        User user = userRepository.findById(id).orElseGet(User::new);
        ValidationUtils.isNull(user.getId(), "User", "id", id);
        return conversionService.convert(user, UserDto.class);
    }

    @Override
//  先简单处理，清理该域中所有缓存
    // 在配置Cacheable时，可以即配置cacheNames又配置key，若将cacheNames视为一个Map，key就是其中单个key
    // 在配置CacheEvict时，若只配置cacheNames，而未配置key，则需要将allEntries = true，否则可能不生效
    // 这个也容易理解，因为cacheNames是一个很大的Cache，可能多个class共同使用，而key的gen是默认带上当前class信息的。主要是为了避免误清理过多的Cache
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(User resources) {
        // 个人认为，对IO密集型的业务，比如SearchDB，可以用Virtual Threads来优化
        ConcurrencyUtils.structuredExecute(null, o -> userRepository.save(resources), () -> {
            if (Objects.isNull(userRepository.findByUsername(resources.getUsername())))
                return true;
            throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "username", resources.getUsername(), "existed"));
        }, () -> {
            if (Objects.isNull(userRepository.findByEmail(resources.getEmail())))
                return true;
            throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "email", resources.getEmail(), "existed"));
        }, () -> {
            if (Objects.isNull(userRepository.findByPhone(resources.getPhone())))
                return true;
            throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "phone", resources.getPhone(), "existed"));
        });

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void update(User resources) throws Exception {
        User user = userRepository.findById(resources.getId()).orElseGet(User::new);
        ValidationUtils.isNull(user.getId(), "User", "id", resources.getId());
        ConcurrencyUtils.structuredExecute(null, o -> {
            updateUserParams(resources, user);
            userRepository.save(user);

            // 发布用户更新事件
            publishUserEvent(resources);
            // 清除本地缓存
            flushCache(user.getUsername());
        }, () -> {
            User user1 = userRepository.findByUsername(resources.getUsername());
            if (user1 != null && !user.getId().equals(user1.getId()))
                throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "username", resources.getUsername(), "existed"));
            return true;
        }, () -> {
            User user2 = userRepository.findByEmail(resources.getEmail());
            if (user2 != null && !user.getId().equals(user2.getId()))
                throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "email", resources.getEmail(), "existed"));
            return true;
        }, () -> {
            User user3 = userRepository.findByPhone(resources.getPhone());
            if (user3 != null && !user.getId().equals(user3.getId()))
                throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "phone", resources.getPhone(), "existed"));
            return true;
        });
//        var convertString4BlobUtil = new ConvertString4BlobUtil<User>();
//        不确定是否需要进行赋值。理论上传递的是引用。更改会影响到这方
//        convertString4BlobUtil.convert(user);

    }

    private static void updateUserParams(User resources, User user) {
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
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @CacheEvict(allEntries = true)
    public void updateCenter(User resources) {
        ConcurrencyUtils.structuredExecute(users -> {
                    var user1 = users.get(0);
                    var user2 = users.get(1);
                    // 两个都存在，且id不相同，则为已存在
                    if (Objects.nonNull(user1) && Objects.nonNull(user2) && !Objects.equals(((User) user1).getId(), ((User) user2).getId()))
                        throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(User.class, "phone", resources.getPhone(), "existed"));
                    return Objects.nonNull(user1) ? user1 : user2;
                }, obj -> {
                    var user = (User) obj;
                    user.setNickName(resources.getNickName());
                    user.setPhone(resources.getPhone());
                    user.setGender(resources.getGender());
                    userRepository.save(user);
                },
                () -> userRepository.findById(resources.getId()).orElseGet(User::new),
                () -> userRepository.findByPhone(resources.getPhone()));
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
        userLocalCache.cleanAll();
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    @Cacheable(key = "'userInfo:' + #p0")
    public UserDto findByName(String userName) {
        User user = userRepository.findByUsername(userName);
        // 这里只做标记用，当前业务暂不需要Projection
        if (RandomUtil.randomBoolean()) {
            var upj = userRepository.findByUsername(userName, UserProj.class);
        }
        if (Objects.isNull(user))
            throw new EntityNotFoundException(ExceptionMsgUtils.generateExcMsg(User.class, "name", userName, "NotExist"));
        else
            return conversionService.convert(user, UserDto.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public UserInnerDto findInnerUserByName(String userName) {
        // 方法内调用，Spring aop不会生效，所以若直接调 findByName(String username) 方法不会走缓存
        var user = userRepository.findByUsername(userName);
        if (Objects.isNull(user))
            throw new EntityNotFoundException(ExceptionMsgUtils.generateExcMsg(User.class, "name", userName, "NotExist"));
        else
            return conversionService.convert(user, UserInnerDto.class);
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
        FileUtils.checkSize(properties.getAvatarMaxSize(), multipartFile.getSize());
        // 验证文件上传的格式
        String image = "gif jpg png jpeg";
        String fileType = FileUtils.getExtensionName(multipartFile.getOriginalFilename());
        if (ObjectUtil.isNotNull(fileType) && !image.contains(fileType)) {
            throw new BadRequestException("文件格式错误！, 仅支持 " + image + " 格式");
        }
        User user = userRepository.findByUsername(SecurityUtils.getCurrentUsername());
        String oldPath = user.getAvatarPath();
        File file = FileUtils.upload(multipartFile, properties.getOSPath().getAvatar());
        user.setAvatarPath(Objects.requireNonNull(file).getPath());
        user.setAvatarName(file.getName());
        userRepository.save(user);
        if (StringUtils.isNotBlank(oldPath)) {
            FileUtils.del(oldPath);
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
        userRepository.updateEnabled(username, !enabled, enabled);
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
        FileUtils.downloadExcel(list, response);
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
    public void setApplicationEventPublisher(@NotNull ApplicationEventPublisher applicationEventPublisher) {
        this.eventPublisher = applicationEventPublisher;
    }

    public void publishUserEvent(User user) {
        eventPublisher.publishEvent(new UserEvent(this, user));
    }

    // 需注意，这里方法的参数是Event对象。另点击方法旁的标志可以查看publisher和listener
    @EventListener
    public void objUpdate(RoleEvent roleEvent) {
        userRepository.findByRoleId(roleEvent.getDataId()).forEach(user -> {
            userLocalCache.cleanUserCache(user.getUsername(), true);
            redisUtils.delInRC(SysCacheKey.USER_ID, user.getId());
            publishUserEvent(user);
        });
    }

    @EventListener
    public void objUpdate(MenuEvent menuEvent) {
        userRepository.findByMenuId(menuEvent.getDataId()).forEach(user -> {
            userLocalCache.cleanUserCache(user.getUsername(), true);
            redisUtils.delInRC(SysCacheKey.USER_ID, user.getId());
            publishUserEvent(user);
        });
    }

    @EventListener
    public void objUpdate(DeptEvent deptEvent) {
        userRepository.findByRoleDeptId(deptEvent.getDataId()).forEach(user -> {
            userLocalCache.cleanUserCache(user.getUsername(), true);
            redisUtils.delInRC(SysCacheKey.USER_ID, user.getId());
            publishUserEvent(user);
        });
    }
}
