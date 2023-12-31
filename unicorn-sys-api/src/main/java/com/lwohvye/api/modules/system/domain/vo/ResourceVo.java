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

package com.lwohvye.api.modules.system.domain.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.core.utils.JDKUtils;
import lombok.Getter;
import lombok.SneakyThrows;

import java.lang.reflect.Field;

@Getter // 要序列化，就要把get方法放出来
public class ResourceVo {
    /**
     * ID
     */
    private Long resourceId;

    /**
     * 资源名称
     */
    private String name;

    // 请求方法。空表示全部
    private String reqMethod;

    /**
     * 状态 0-不可用 1-可用
     */
    private Boolean status;

    @JsonIgnore
    @SneakyThrows
    public static ResourceVo toVo(Resource resource) {
        // 在这里打断点，导致发起了很多打dbSearch，造成长时间停顿。不打就没问题
        var vo = new ResourceVo();
        var fields = ResourceVo.class.getDeclaredFields();
        for (Field field : fields) JDKUtils.copyFieldVal(resource, vo, field);
//        for (Field field : fields) {
//            var rField = ReflectionUtils.findField(resource.getClass(), field.getName());
//            if (Objects.nonNull(rField) && field.trySetAccessible() && rField.trySetAccessible())
//                field.set(vo, rField.get(resource));
        // ReflectUtil.setFieldValue(vo, field, ReflectUtil.getFieldValue(resource, field.getName()));
//        }
        return vo;
    }
}
