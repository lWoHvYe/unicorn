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
package com.lwohvye.base;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.context.CycleAvoidingMappingContext;
import org.mapstruct.Context;
import org.mapstruct.InheritInverseConfiguration;

import java.util.List;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
public interface BaseMapper<D, E> {
/*
    @Mapper 表示该接口作为映射接口，编译时MapStruct处理器的入口。
    @Mappings 一组映射关系
    @Mapping 一对映射关系，target：目标类字段，source ：源字段，expression ：target字段使用改表达式获取值
    @InheritInverseConfiguration，表示方法继承相应的反向方法的反向配置。比如在toDto上配了转换逻辑，在toEntity加上该注解即可完成相关转换逻辑对配置
    @InheritConfiguration，指定映射方法
 */

    /**
     * DTO转Entity
     *
     * @param dto /
     * @return /
     */
    @InheritInverseConfiguration
    E toEntity(D dto, @Context CycleAvoidingMappingContext context);

    /**
     * Entity转DTO
     *
     * @param entity /
     * @return /
     */
    D toDto(E entity, @Context CycleAvoidingMappingContext context);

    /**
     * DTO集合转Entity集合
     *
     * @param dtoList /
     * @param context
     * @return /
     */
    @InheritInverseConfiguration
    List<E> toEntity(List<D> dtoList, @Context CycleAvoidingMappingContext context);

    /**
     * Entity集合转DTO集合
     *
     * @param entityList /
     * @param context
     * @return /
     */
    List<D> toDto(List<E> entityList, @Context CycleAvoidingMappingContext context);

    //    通用的JSONObject/JSONArray与String互转的方法。当入是A出是B时，会自动调用相关的规则，这里配置会作为默认的转换规则。欲使用其他的，可使用@Mapping(target="",expression="java(method...)")
    default JSONObject convertString2JSONObject(String in) {
        return StrUtil.isNotBlank(in) ? JSONObject.parseObject(in) : new JSONObject();
    }

    default JSONArray convertString2JSONArray(String in) {
        return StrUtil.isNotBlank(in) ? JSONArray.parseArray(in) : new JSONArray();
    }

    default String convertJSON2String(JSON in) {
        return ObjectUtil.isNotNull(in) ? in.toJSONString() : "";
    }
}
