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
package com.lwohvye.modules.system.service.mapstruct;

import com.lwohvye.base.BaseMapper;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.modules.system.domain.Resource;
import com.lwohvye.modules.system.service.dto.ResourceDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.ReportingPolicy;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
@Mapper(componentModel = "spring", unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface ResourceMapper extends BaseMapper<ResourceDto, Resource> {

    @Override
    @Mapping(target = "roleCodes", expression = "java(entity.getRoles().stream()" +
                                                ".map(com.lwohvye.modules.system.domain.Role::getCode)" +
                                                ".collect(java.util.stream.Collectors.joining(\",\")))")
    ResourceDto toDto(Resource entity, CycleAvoidingMappingContext context);

}
