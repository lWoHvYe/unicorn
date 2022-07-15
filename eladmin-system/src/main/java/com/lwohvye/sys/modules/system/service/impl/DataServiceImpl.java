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

import com.lwohvye.api.modules.system.domain.Dept;
import com.lwohvye.api.modules.system.domain.User;
import com.lwohvye.api.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.sys.modules.system.event.UserEvent;
import com.lwohvye.sys.modules.system.service.IDataService;
import com.lwohvye.sys.modules.system.service.IDeptService;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.utils.CacheKey;
import com.lwohvye.utils.enums.DataScopeEnum;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 数据权限服务实现
 *
 * @author Zheng Jie
 * @website https://el-admin.vip
 * @date 2020-05-07
 **/
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "data")
public class DataServiceImpl implements IDataService {

    private final IRoleService roleService;
    private final IDeptService deptService;
    private final RedisUtils redisUtils;

    /**
     * 用户角色改变时需清理缓存
     *
     * @param userId /
     * @param deptId /
     * @return /
     */
    @Override
    @Cacheable(key = " #root.target.getSysName() + 'user:' + #p0")
    public List<Long> getDeptIds(Long userId, Long deptId) {
        // 用于存储部门id
        Set<Long> deptIds = new HashSet<>();
        // 查询用户角色
        List<RoleSmallDto> roleSet = roleService.findByUserId(userId);
        // 获取对应的部门ID
        for (RoleSmallDto role : roleSet) {
            DataScopeEnum dataScopeEnum = DataScopeEnum.find(role.getDataScope());
            switch (Objects.requireNonNull(dataScopeEnum)) {
                case THIS_LEVEL:
                    deptIds.add(deptId);
                    break;
                case CUSTOMIZE:
                    deptIds.addAll(getCustomize(deptIds, role));
                    break;
                default:
                    return new ArrayList<>(deptIds);
            }
        }
        return new ArrayList<>(deptIds);
    }

    /**
     * 获取自定义的数据权限
     *
     * @param deptIds 部门ID
     * @param role    角色
     * @return 数据权限ID
     */
    public Set<Long> getCustomize(Set<Long> deptIds, RoleSmallDto role) {
        Set<Dept> depts = deptService.findByRoleId(role.getId());
        for (Dept dept : depts) {
            deptIds.add(dept.getId());
            List<Dept> deptChildren = deptService.findByPid(dept.getId());
            if (deptChildren != null && !deptChildren.isEmpty()) {
                deptIds.addAll(deptService.getDeptChildren(deptChildren));
            }
        }
        return deptIds;
    }

    @EventListener
    public void objUpdate(UserEvent userEvent) {
        redisUtils.delInRC(CacheKey.DATA_USER, userEvent.getEventData().getId());
    }
}
