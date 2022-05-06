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

import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.utils.json.JsonUtils;
import org.mapstruct.Context;
import org.mapstruct.InheritInverseConfiguration;
import org.springframework.core.convert.converter.Converter;

import java.util.List;
import java.util.Map;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
public interface BaseMapper<D, E> extends Converter<E, D> {
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
     * Entity转DTO，由Spring的Converter替换。这个只能单向转换，当都是这个方向时，用这个可以减少Mapper的注入
     * @see https://mapstruct.org/news/2022-02-07-mapstruct-spring-extensions-0-1-1-released/
     *
     * @param entity /
     * @return /
     */
    // D toDto(E entity, @Context CycleAvoidingMappingContext context);


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

    // 为了方便，可能在配置@Mapper时，使用uses，这时方法就不知道要用那个mapper里的了。这里加了name,除非显示使用qualifiedByName，否则不会使用，
    // 但针对当前问题中不能使用qualifiedByName,因为这些mapper中都有这方法，又回到源点来，建议在对应但Mapper中Override一下。
    // 若在项目中不用uses相互引用mapper，可以把@Named移除掉，在有需要时会自动使用
    // https://mapstruct.org/documentation/stable/reference/html/#selection-based-on-qualifiers
    // 通用的Map/List与Json-String互转的方法。当入是A出是B时，会自动调用相关的规则，这里配置会作为默认的转换规则。欲使用其他的，可使用@Mapping(target="",expression="java(method...)")
    default Map convertString2JsonMap(String in) {
        return JsonUtils.toMap(in);
    }

    default List convertString2JsonList(String in) {
        return JsonUtils.toList(in);
    }

    default String convertMap2String(Map in) {
        return JsonUtils.toJSONString(in);
    }

    default String convertList2String(List in) {
        return JsonUtils.toJSONString(in);
    }
}
