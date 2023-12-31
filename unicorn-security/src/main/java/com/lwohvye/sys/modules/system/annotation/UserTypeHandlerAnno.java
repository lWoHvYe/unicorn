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
package com.lwohvye.sys.modules.system.annotation;

import com.lwohvye.sys.modules.system.enums.UserTypeEnum;

import java.lang.annotation.*;

/**
 * 用户类型注解
 *
 * @author Hongyan Wang
 * @date 2021年11月02日 16:47
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface UserTypeHandlerAnno {
    UserTypeEnum value() default UserTypeEnum.EXTRA;

    // 扩展使用
    String typeName() default "";
}
