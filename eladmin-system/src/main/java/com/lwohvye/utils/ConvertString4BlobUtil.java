package com.lwohvye.utils;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.annotation.String4Blob;

import java.util.Arrays;

/**
 * @author Hongyan Wang
 * @description 将实体中乱码的String正常转码
 * @date 2021年04月01日 0:13
 */
public class ConvertString4BlobUtil<T> {
    public T convert(T t) {
//        return StringUtils.convertToString(in);
//        便利所有属性
        Arrays.stream(ReflectUtil.getFields(t.getClass())).forEach(field -> {
//            看是否有指定的注解
            var annotation = field.getAnnotation(String4Blob.class);
            if (ObjectUtil.isNotNull(annotation)) {
                var value = ReflectUtil.getFieldValue(t, field);
//                只处理String 类型的值
                if (ObjectUtil.isNotEmpty(value) && value instanceof String) {
//                    正确转码一下
                    var convertVal = StringUtils.convertToString(String.valueOf(value));
//                    设置值
                    ReflectUtil.setFieldValue(t, field, convertVal);
                }
            }
        });
        return t;
    }
}
