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
package com.lwohvye.utils;

import cn.hutool.core.util.StrUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.env.Environment;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author Jie, Super Idol lv, Super Idol peng
 * @description 可用来获取IOC注册的Bean
 * 使用 Class.forName(String s)时，传的是类的全路径（包含包）
 * 上面几种获取bean的。传的是bean的名称（首字母小写）
 * 针对接口，需要获取相关的实现类，因为注解是在实现类上的
 * @date 2019-01-07
 */
@Slf4j
@SuppressWarnings("unused")
public class SpringContextHolder implements ApplicationContextAware, DisposableBean {

    //    Spring应用上下文环境
    private static ApplicationContext applicationContext = null;
    private static final List<CallBack> CALL_BACKS = new ArrayList<>();
    private static boolean addCallback = true;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        if (!Objects.isNull(SpringContextHolder.applicationContext))
            log.warn("SpringContextHolder中的ApplicationContext被覆盖, 原有ApplicationContext为: {}", SpringContextHolder.applicationContext);

        SpringContextHolder.applicationContext = applicationContext;
        if (addCallback) {
            for (CallBack callBack : SpringContextHolder.CALL_BACKS)
                callBack.executor();
            CALL_BACKS.clear();
        }
        SpringContextHolder.addCallback = false;
    }

    public static ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    @Override
    public void destroy() {
        SpringContextHolder.clearHolder();
    }

    /**
     * 针对 某些初始化方法，在SpringContextHolder 未初始化时 提交回调方法。
     * 在SpringContextHolder 初始化后，进行回调使用
     *
     * @param callBack 回调函数
     */
    public static synchronized void addCallBacks(CallBack callBack) {
        if (addCallback) {
            SpringContextHolder.CALL_BACKS.add(callBack);
        } else {
            log.warn("CallBack：{} 已无法添加！立即执行", callBack.getCallBackName());
            callBack.executor();
        }
    }

    /**
     * 从静态变量applicationContext中取得Bean, 自动转型为所赋值对象的类型.
     */
    @SuppressWarnings("unchecked")
    public static <T> T getBean(String beanName) {
        assertContextInjected();
        return (T) applicationContext.getBean(beanName);
    }

    /**
     * 从静态变量applicationContext中取得Bean, 自动转型为所赋值对象的类型.
     */
    public static <T> T getBean(Class<T> requiredType) {
        assertContextInjected();
        return applicationContext.getBean(requiredType);
    }

    /**
     * 拿到ApplicationContext对象实例后就可以手动获取Bean的注入实例对象
     */
    public static <T> T getBean(String beanName, Class<T> clazz) {
        if (StrUtil.isBlank(beanName)) {
            return applicationContext.getBean(clazz);
        } else {
            return applicationContext.getBean(beanName, clazz);
        }
    }

    public static Object getBean(String beanName, String className) throws ClassNotFoundException {
        Class<?> clz = Class.forName(className);
        return applicationContext.getBean(beanName, clz);
    }

    /**
     * @param clazz 类型
     * @return java.util.Map key为beanName，value为beanInstance
     * @description 根据Class获取所有该类型的Bean，可用于获取类的所有子类、接口的所有实现类
     * @date 2021/11/23 11:48 上午
     */
    public static <T> Map<String, T> getBeansOfType(Class<T> clazz) {
        return applicationContext.getBeansOfType(clazz);
    }

    /**
     * @param beanName /
     * @return boolean
     * @description 检查ApplicationContext中是否包含
     * @date 2021/11/23 9:33 上午
     */
    public static boolean containsBean(String beanName) {
        return applicationContext.containsBean(beanName);
    }

    /**
     * @param name /
     * @return boolean
     * @description 判断bean是否为单例
     * @date 2021/11/23 9:34 上午
     */
    public static boolean isSingleton(String name) throws NoSuchBeanDefinitionException {
        return applicationContext.isSingleton(name);
    }

    /**
     * @param beanName /
     * @return java.lang.Class
     * @description 获取bean的类型
     * @date 2021/11/23 9:35 上午
     */
    public static Class<?> getType(String beanName) throws NoSuchBeanDefinitionException {
        return applicationContext.getType(beanName);
    }

    public static String[] getAliases(String name) throws NoSuchBeanDefinitionException {
        return applicationContext.getAliases(name);
    }

    /**
     * 获取SpringBoot 配置信息，并可设置默认值
     *
     * @param property     属性key
     * @param defaultValue 默认值
     * @param requiredType 返回类型
     * @return /
     */
    public static <T> T getProperties(String property, T defaultValue, Class<T> requiredType) {
        T result = defaultValue;
        try {
            result = getBean(Environment.class).getProperty(property, requiredType);
        } catch (Exception ignored) {
            // 名为ignored 的变量即为忽略
        }
        return result;
    }

    /**
     * 获取SpringBoot 配置信息
     *
     * @param property 属性key
     * @return /
     */
    public static String getProperties(String property) {
        return getProperties(property, null, String.class);
    }

    /**
     * 获取SpringBoot 配置信息
     *
     * @param property     属性key
     * @param requiredType 返回类型
     * @return /
     */
    public static <T> T getProperties(String property, Class<T> requiredType) {
        return getProperties(property, null, requiredType);
    }

    /**
     * 检查ApplicationContext不为空.
     */
    private static void assertContextInjected() {
        if (Objects.isNull(applicationContext))
            throw new IllegalStateException("applicationContext属性未注入, 请在applicationContext.xml中定义SpringContextHolder或在SpringBoot启动类中注册SpringContextHolder.");
    }

    /**
     * 清除SpringContextHolder中的ApplicationContext为Null.
     */
    private static void clearHolder() {
        log.debug("清除SpringContextHolder中的ApplicationContext: {}", applicationContext);
        applicationContext = null;
    }
}
