/*
 *  Copyright 2019-2022 lWoHvYe
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
package com.lwohvye.sys.modules.system.service.mapstruct;

import com.lwohvye.base.BaseMapper;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.api.modules.system.service.dto.ResourceDto;
import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.ReportingPolicy;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
@Mapper(componentModel = "spring", unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface ResourceMapper extends BaseMapper<ResourceDto, Resource> {

    @Override
    @Mapping(target = "roleCodes", expression = "java(genSimpleRole(entity.getRoles()))")
        // ⚠️ 若不加@Context，则toDto(List<T> list) 不会用 toDto(T t)。
    ResourceDto toDto(Resource entity, @Context CycleAvoidingMappingContext context);

    default List<String> genSimpleRole(Collection<Role> roles) {
        if (Objects.isNull(roles))
            return Collections.emptyList();
        return roles.stream().map(role -> role.getCode().toUpperCase()).toList();
    }
}
