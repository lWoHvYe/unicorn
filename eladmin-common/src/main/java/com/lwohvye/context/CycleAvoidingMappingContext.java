/*
 * Copyright MapStruct Authors.
 *
 * Licensed under the Apache License version 2.0, available at http://www.apache.org/licenses/LICENSE-2.0
 */
package com.lwohvye.context;

import org.mapstruct.BeforeMapping;
import org.mapstruct.Context;
import org.mapstruct.MappingTarget;
import org.mapstruct.TargetType;

import java.util.IdentityHashMap;
import java.util.Map;

/**
 * A type to be used as {@link Context} parameter to track cycles in graphs.
 * <p>
 * Depending on the actual use case, the two methods below could also be changed to only accept certain argument types,
 * e.g. base classes of graph nodes, avoiding the need to capture any other objects that wouldn't necessarily result in
 * cycles.
 * https://github.com/mapstruct/mapstruct/issues/469
 * 这里主要解决了转dto时的循环依赖问题。但在toString()时(例如先前端返回，转Json)还是会出现栈溢出（官方但example调toString()也是一样）。所以解决的最根本方法还是定义两个Dto来主观的断开循环链
 * 可参考 https://github.com/lWoHvYe/spring-boot-jpa-cascade/tree/main/src/main/java/com/lwohvye/modules/content/service/dto
 * 中的BossProductDTO、BossProductServiceDTO和BossProductSmallDTO。 smallDTO就是用来中断循环链的
 * 收尾：https://www.lwohvye.com/2020/12/01/manytomany%e6%88%96onetomany-manytoone%e5%af%bc%e8%87%b4%e5%be%aa%e7%8e%af%e4%be%9d%e8%b5%96%e7%9a%84%e9%97%ae%e9%a2%98-java-lang-stackoverflowerror-jpa/
 *
 * @author Andreas Gudian
 */
public class CycleAvoidingMappingContext {
    private Map<Object, Object> knownInstances = new IdentityHashMap<>();

    @BeforeMapping
    public <T> T getMappedInstance(Object source, @TargetType Class<T> targetType) {
        return (T) knownInstances.get(source);
    }

    @BeforeMapping
    public void storeMappedInstance(Object source, @MappingTarget Object target) {
        knownInstances.put(source, target);
    }
}
