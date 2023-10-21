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
import com.lwohvye.api.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.sys.modules.system.event.RoleEvent;
import com.lwohvye.sys.modules.system.event.UserEvent;
import com.lwohvye.sys.modules.system.service.IDataService;
import com.lwohvye.sys.modules.system.service.IDeptService;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.sys.common.constant.SysCacheKey;
import com.lwohvye.core.enums.DataScopeEnum;
import com.lwohvye.core.utils.redis.RedisUtils;
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
    @Cacheable(key = "'user:' + #p0")
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

    @Override
    @Cacheable(key = "'data_scope4user:' + #p0")
    public String getDataScope(Long userId) {
        var dataScopes = roleService.findByUserId(userId).stream().map(RoleSmallDto::getDataScope).toList();
        if (dataScopes.contains(DataScopeEnum.ALL.getValue()))
            return DataScopeEnum.ALL.getValue();
        else if (dataScopes.contains(DataScopeEnum.CUSTOMIZE.getValue()))
            return DataScopeEnum.CUSTOMIZE.getValue();
        else if (dataScopes.contains(DataScopeEnum.THIS_LEVEL.getValue()))
            return DataScopeEnum.THIS_LEVEL.getValue();
        else return "";
    }

    /**
     * 获取自定义的数据权限
     *
     * @param deptIds 部门ID
     * @param role    角色
     * @return 数据权限ID
     */
    private Set<Long> getCustomize(Set<Long> deptIds, RoleSmallDto role) {
        Set<Dept> depts = deptService.findByRoleId(role.getId());
        for (Dept dept : depts) {
            deptIds.add(dept.getId());
            deptIds.addAll(deptService.fetchDeptChildByPid(dept.getId()));
        }
        return deptIds;
    }

    @EventListener
    public void objUpdate(UserEvent userEvent) {
        redisUtils.delInRC(SysCacheKey.DATA_USER, userEvent.getDataId());
        redisUtils.delInRC(SysCacheKey.DATA_SCOPE, userEvent.getDataId());
    }

    @EventListener
    public void objUpdate(RoleEvent roleEvent) {
        redisUtils.delInRC(SysCacheKey.DATA_USER, null);
        redisUtils.delInRC(SysCacheKey.DATA_SCOPE, null);
    }
}
