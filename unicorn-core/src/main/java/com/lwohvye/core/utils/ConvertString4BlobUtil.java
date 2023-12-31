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
package com.lwohvye.core.utils;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.core.annotation.String4Blob;

import java.util.Arrays;

/**
 * 将实体中乱码的String正常转码
 *
 * @author Hongyan Wang
 * @date 2021年04月01日 0:13
 */
public class ConvertString4BlobUtil<T> {
    public T convert(T t) {
//        便利所有属性
        Arrays.stream(ReflectUtil.getFields(t.getClass())).forEach(field -> {
//            看是否有指定的注解
            var annotation = field.getAnnotation(String4Blob.class);
            if (ObjectUtil.isNotNull(annotation)) {
                var value = ReflectUtil.getFieldValue(t, field);
//                只处理String 类型的值
                if (ObjectUtil.isNotEmpty(value) && value instanceof String str) {
//                    正确转码一下
                    var convertVal = StringUtils.convertToString(str);
//                    设置值
                    ReflectUtil.setFieldValue(t, field, convertVal);
                }
            }
        });
        return t;
    }
}
