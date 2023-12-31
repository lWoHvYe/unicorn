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
package com.lwohvye.sys.common.condition;

import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.type.AnnotatedTypeMetadata;

public class MacOSCondition implements Condition {
    /**
     * @param context:判断条件能使用的上下文环境
     * @param metadata:注解所在位置的注释信息
     */
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        //获取ioc使用的beanFactory
        var beanFactory = context.getBeanFactory();
        //获取类加载器
        var classLoader = context.getClassLoader();
        //获取当前环境信息
        var environment = context.getEnvironment();
        //获取bean定义的注册类
        var registry = context.getRegistry();

        //获得当前系统名
        String property = environment.getProperty("os.name");
        return environment.getProperty("os.name", "Darwin").contains("Mac OS X");
    }
}
