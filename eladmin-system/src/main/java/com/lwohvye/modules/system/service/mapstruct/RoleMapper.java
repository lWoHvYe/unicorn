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
package com.lwohvye.modules.system.service.mapstruct;

import com.lwohvye.base.BaseMapper;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.api.modules.system.domain.vo.ResourceVo;
import com.lwohvye.api.modules.system.service.dto.RoleDto;
import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.ReportingPolicy;

import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Mapper(componentModel = "spring", uses = {MenuMapper.class, DeptMapper.class}, unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface RoleMapper extends BaseMapper<RoleDto, Role> {

    @Override
    @Mapping(target = "resourcesOt", expression = "java(toRVo(entity.getResources()))")
    RoleDto toDto(Role entity, @Context CycleAvoidingMappingContext context);

    default Set<ResourceVo> toRVo(Set<Resource> resources) {
        return resources.stream().map(ResourceVo::toVo).collect(Collectors.toSet());
    }
}
