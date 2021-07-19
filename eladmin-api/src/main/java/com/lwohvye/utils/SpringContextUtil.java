/*
 *  Copyright 2020-2022 lWoHvYe
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
package com.lwohvye.utils;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

/**
 * @author Super Idol lv
 * @description 可用来获取IOC注册的Bean
 * 使用 Class.forName(String s)时，传的是类的全路径（包含包）
 * 上面几种获取bean的。传的是bean的名称（首字母小写）
 * 针对接口，需要获取相关的实现类，因为注解是在实现类上的
 * @date 2021-04-30
 */
@Component
public class SpringContextUtil implements ApplicationContextAware {
    //    Spring应用上下文环境
    private static ApplicationContext applicationContext;

//    public SpringContextUtil() {
//    }

    public void setApplicationContext(ApplicationContext arg0) throws BeansException {
        if (ObjectUtil.isNull(applicationContext))
            applicationContext = arg0;
    }

    public static ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    public static void setAppCtx(ApplicationContext webAppCtx) {
        if (ObjectUtil.isNotNull(webAppCtx))
            applicationContext = webAppCtx;
    }

    /**
     * 拿到ApplicationContext对象实例后就可以手动获取Bean的注入实例对象
     */
    public static <T> T getBean(Class<T> clazz) {
        return getApplicationContext().getBean(clazz);
    }

    public static <T> T getBean(String name, Class<T> clazz) {
        if (StrUtil.isBlank(name)) {
            return getApplicationContext().getBean(clazz);
        } else {
            return getApplicationContext().getBean(name, clazz);
        }
    }

    public static Object getBean(String beanName) {
        return getApplicationContext().getBean(beanName);
    }

    public static Object getBean(String beanName, String className) throws ClassNotFoundException {
        Class<?> clz = Class.forName(className);
        return getApplicationContext().getBean(beanName, clz);
    }

    public static boolean containsBean(String name) {
        return getApplicationContext().containsBean(name);
    }

    public static boolean isSingleton(String name) throws NoSuchBeanDefinitionException {
        return getApplicationContext().isSingleton(name);
    }

    public static Class<?> getType(String name) throws NoSuchBeanDefinitionException {
        return getApplicationContext().getType(name);
    }

    public static String[] getAliases(String name) throws NoSuchBeanDefinitionException {
        return getApplicationContext().getAliases(name);
    }
}
