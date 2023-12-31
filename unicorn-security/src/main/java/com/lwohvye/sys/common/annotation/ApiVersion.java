/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.common.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.web.bind.annotation.Mapping;

/**
 * 自定义版本号标记注解 @ApiVersion
 */
@Target({
        ElementType.METHOD, //接口用于方法上
        ElementType.TYPE    //接口用于类上
})
@Retention(RetentionPolicy.RUNTIME) //只在运行时才有效
@Documented //标识这是个注解并应该被 javadoc工具记录
@Mapping //标识映射
public @interface ApiVersion {
    /**
     * 标识版本号
     */
    int value() default 1;
}
