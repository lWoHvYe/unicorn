/*
 * Copyright MapStruct Authors.
 *
 * Licensed under the Apache License version 2.0, available at http://www.apache.org/licenses/LICENSE-2.0
 */
package com.lwohvye.context;

import cn.hutool.core.util.ReflectUtil;
import lombok.SneakyThrows;
import org.mapstruct.BeforeMapping;
import org.mapstruct.Context;
import org.mapstruct.MappingTarget;
import org.mapstruct.TargetType;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A type to be used as {@link Context} parameter to track cycles in graphs.
 * <p>
 * Depending on the actual use case, the two methods below could also be changed to only accept certain argument types,
 * e.g. base classes of graph nodes, avoiding the need to capture any other objects that wouldn't necessarily result in
 * cycles.
 * <a href="https://github.com/mapstruct/mapstruct/issues/469">...</a>
 * 这里主要解决了转dto时的循环依赖问题。但在toString()时(例如先前端返回，转Json)还是会出现栈溢出（官方但example调toString()也是一样）。所以解决的最根本方法还是定义两个Dto来主观的断开循环链
 * 可参考 <a href="https://github.com/lWoHvYe/spring-boot-jpa-cascade/tree/main/src/main/java/com/lwohvye/modules/content/service/dto">...</a>
 * 中的BossProductDTO、BossProductServiceDTO和BossProductSmallDTO。 smallDTO就是用来中断循环链的
 * 收尾：<a href="https://www.lwohvye.com/2020/12/01/manytomany%e6%88%96onetomany-manytoone%e5%af%bc%e8%87%b4%e5%be%aa%e7%8e%af%e4%be%9d%e8%b5%96%e7%9a%84%e9%97%ae%e9%a2%98-java-lang-stackoverflowerror-jpa/">...</a>
 *
 * @author Andreas Gudian
 * @since 2.6.16
 */
public class CycleAvoidingMappingContext {
    // 因为是私有的，似乎是没太大用途
    private final Map<Object, Object> knownInstances = new IdentityHashMap<>();

    @BeforeMapping
    public <T> T getMappedInstance(Object source, @TargetType Class<T> targetType) {
        var obj = knownInstances.get(source);
        // 判断类型
        if (targetType.isInstance(obj))
            // 是该类型进行转换
            return targetType.cast(obj);
        else
            // 不是该类型时，一般是small类型，需要做source -> T的显示转换(非强转)
            return genT(targetType, obj);
    }

    /**
     * smallDto中的属性，必须为原始侧的子集
     *
     * @param targetType Class for smallDto
     * @param obj        原始侧Dto
     * @return T         smallDto的实例
     * @date 2021/11/10 12:38 上午
     */
    @SneakyThrows
    private <T> T genT(Class<T> targetType, Object obj) {
        // obj为null时，直接返回
        if (Objects.isNull(obj))
            return null;

        // var t = ReflectUtil.newInstance(targetType);
        var t = targetType.getDeclaredConstructor().newInstance();
        // targetType.getFields()只能获取到非私有的属性。所以还是需要反射来获取
        // targetType.getDeclaredFields() 可以获取本类中的所有域，不包括从超类继承的
        // 所以还是使用ReflectUtil.getFields(targetType)，获取全部的域，包括从超类继承的
        for (Field field : ReflectUtil.getFields(targetType)) {
            // 获取不到属性会报错哦。并且需注意，从obj取时，要使用fieldName，因为field是t中的属性
            var oField = ReflectionUtils.findField(obj.getClass(), field.getName());
            if (Objects.nonNull(oField) && oField.trySetAccessible() && field.trySetAccessible())
                field.set(t, oField.get(obj));
            // 下面的反射，底层还是 field.get()获取属性、field.set()设置属性
            // ReflectUtil.setFieldValue(t, field, ReflectUtil.getFieldValue(obj, field.getName()));
        }
        // 不是该类型，通过先转成Json，再转成另一实体实现。这种不一致的一般是用xxxSmallDTO时。这是不使用反射时，另一种处理方式
        // return JsonUtils.toJavaObject(obj, targetType);
        return t;
    }

    /**
     * 存储进map中
     *
     * @param source key
     * @param target val
     * @date 2021/12/6 2:32 PM
     */
    @BeforeMapping
    public void storeMappedInstance(Object source, @MappingTarget Object target) {
        knownInstances.put(source, target);
    }
}
