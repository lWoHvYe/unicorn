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

import cn.hutool.core.collection.CollectionUtil;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.exception.EntityExistException;
import com.lwohvye.modules.security.service.UserCacheClean;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.domain.User;
import com.lwohvye.modules.system.handler.AuthHandlerContext;
import com.lwohvye.modules.system.repository.RoleRepository;
import com.lwohvye.modules.system.repository.UserRepository;
import com.lwohvye.modules.system.service.IRoleService;
import com.lwohvye.modules.system.service.dto.RoleDto;
import com.lwohvye.modules.system.service.dto.RoleQueryCriteria;
import com.lwohvye.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.modules.system.service.mapstruct.RoleMapper;
import com.lwohvye.modules.system.service.mapstruct.RoleSmallMapper;
import com.lwohvye.utils.*;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 * @date 2018-12-03
 */
@Slf4j
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "role")
public class RoleServiceImpl implements IRoleService {

    private final RoleRepository roleRepository;
    private final RoleMapper roleMapper;
    private final RoleSmallMapper roleSmallMapper;
    private final RedisUtils redisUtils;
    private final UserRepository userRepository;
    private final AuthHandlerContext authHandlerContext;
    private final UserCacheClean userCacheClean;

    @Override
    @Cacheable(key = " #root.target.getSysName() + 'all-roles'")
    @Transactional(rollbackFor = Exception.class)
    public List<RoleDto> queryAll() {
        Sort sort = Sort.by(Sort.Direction.ASC, "level");
        return roleMapper.toDto(roleRepository.findAll(sort), new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<RoleDto> queryAll(RoleQueryCriteria criteria) {
        return roleMapper.toDto(roleRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder)), new CycleAvoidingMappingContext());
    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public Object queryAll(RoleQueryCriteria criteria, Pageable pageable) {
        Page<Role> page = roleRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtil.toPage(page.map(role -> roleMapper.toDto(role, new CycleAvoidingMappingContext())));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @Cacheable(key = " #root.target.getSysName() + 'id:' + #p0")
    public RoleDto findById(long id) {
        Role role = roleRepository.findById(id).orElseGet(Role::new);
        ValidationUtil.isNull(role.getId(), "Role", "id", id);
        return roleMapper.toDto(role, new CycleAvoidingMappingContext());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(Role resources) {
        if (roleRepository.findByName(resources.getName()) != null) {
            throw new EntityExistException(Role.class, "username", resources.getName());
        }
        roleRepository.save(resources);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(Role resources) {
        Role role = roleRepository.findById(resources.getId()).orElseGet(Role::new);
        ValidationUtil.isNull(role.getId(), "Role", "id", resources.getId());

        Role role1 = roleRepository.findByName(resources.getName());

        if (role1 != null && !role1.getId().equals(role.getId())) {
            throw new EntityExistException(Role.class, "username", resources.getName());
        }
        role.setName(resources.getName());
        role.setDescription(resources.getDescription());
        role.setDataScope(resources.getDataScope());
        role.setDepts(resources.getDepts());
        role.setLevel(resources.getLevel());
        roleRepository.save(role);
        // 更新相关缓存
        delCaches(role.getId(), null);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void updateMenu(Role resources, RoleDto roleDTO) {
        Role role = roleMapper.toEntity(roleDTO, new CycleAvoidingMappingContext());
        List<User> users = userRepository.findByRoleId(role.getId());
        // 更新菜单
        role.setMenus(resources.getMenus());
        delCaches(resources.getId(), users);
        roleRepository.save(role);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void untiedMenu(Long menuId) {
        // 更新菜单
        roleRepository.untiedMenu(menuId);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<Long> ids) {
        for (Long id : ids) {
            // 更新相关缓存
            delCaches(id, null);
        }
        roleRepository.deleteAllByIdIn(ids);
    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public List<RoleSmallDto> findByUsersId(Long id) {
        return roleSmallMapper.toDto(new ArrayList<>(roleRepository.findByUserId(id)), new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer findByRoles(Set<Role> roles) {
        if (roles.isEmpty()) {
            return Integer.MAX_VALUE;
        }
        Set<RoleDto> roleDtos = new HashSet<>();
        for (Role role : roles) {
            roleDtos.add(findById(role.getId()));
        }
        return Collections.min(roleDtos.stream().map(RoleDto::getLevel).toList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
//    当使用root对象的属性作为key时，可以将“#root”省略，因为Spring默认使用的就是root对象的属性。
//    需注意是 target.xxx 不带前面的 #
    @Cacheable(key = " #root.target.getSysName() + 'auth:' + #p0")
    public List<GrantedAuthority> grantedAuthorityGenHandler(Long userId, Boolean isAdmin) {
        // admin对应1，非admin对应0
        var instance = authHandlerContext.getInstance(Boolean.TRUE.equals(isAdmin) ? 1 : 0);
        return instance.handler(userId);
    }

    @Override
    public void download(List<RoleDto> roles, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (RoleDto role : roles) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("角色名称", role.getName());
            map.put("角色级别", role.getLevel());
            map.put("描述", role.getDescription());
            map.put("创建日期", role.getCreateTime());
            list.add(map);
        }
        FileUtil.downloadExcel(list, response);
    }

    @Override
    public void verification(Set<Long> ids) {
        if (userRepository.countByRoles(ids) > 0) {
            throw new BadRequestException("所选角色存在用户关联，请解除关联再试！");
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Role> findInMenuId(List<Long> menuIds) {
        return roleRepository.findInMenuId(menuIds);
    }

    /**
     * 清理缓存
     *
     * @param id /
     */
    public void delCaches(Long id, List<User> users) {
        users = CollectionUtil.isEmpty(users) ? userRepository.findByRoleId(id) : users;
        if (CollectionUtil.isNotEmpty(users)) {
            users.forEach(item -> userCacheClean.cleanUserCache(item.getUsername()));
            Set<Long> userIds = users.stream().map(User::getId).collect(Collectors.toSet());
            redisUtils.delByKeys4Business(CacheKey.DATA_USER, userIds);
            redisUtils.delByKeys4Business(CacheKey.MENU_USER, userIds);
            redisUtils.delByKeys4Business(CacheKey.ROLE_AUTH, userIds);
        }
        redisUtils.delete(CacheKey.ROLE_ID + id);
    }
}
