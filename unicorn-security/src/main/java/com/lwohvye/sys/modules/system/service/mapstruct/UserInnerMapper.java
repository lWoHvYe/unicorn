/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.lwohvye.sys.modules.system.service.mapstruct;

import com.lwohvye.api.modules.system.domain.Job;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.api.modules.system.domain.User;
import com.lwohvye.api.modules.system.service.dto.UserInnerDto;
import com.lwohvye.core.base.BaseMapper;
import org.jetbrains.annotations.NotNull;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.ReportingPolicy;

import java.util.Collection;
import java.util.List;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Mapper(componentModel = "spring", uses = {}, unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface UserInnerMapper extends BaseMapper<UserInnerDto, User> {
    // 针对于MapStruct，推荐一款插件：https://plugins.jetbrains.com/plugin/10036-mapstruct-support
    @Override
//    @Mapping(target = "description", source = "description", qualifiedBy = Blob2String.class)
    @Mapping(target = "roleIds", expression = "java(entity.getRoles().stream().map(this::getRoleId).toList())")
    @Mapping(target = "jobIds", expression = "java(this.getJobIds(entity.getJobs()))")
    @Mapping(target = "deptId", source = "dept.id")
    UserInnerDto convert(@NotNull User entity);

    default Long getRoleId(Role role) {
        return role.getId();
    }

    default List<Long> getJobIds(Collection<Job> jobs) {
        return jobs.stream().map(Job::getId).toList();
    }
}
