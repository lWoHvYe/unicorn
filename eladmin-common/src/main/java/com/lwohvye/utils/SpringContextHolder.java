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

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Repository;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.*;
import java.util.jar.JarFile;

/**
 * 可用来获取IOC注册的Bean
 * 使用 Class.forName(String s)时，传的是类的全路径（包含包）
 * 上面几种获取bean的。传的是bean的名称（首字母小写）
 * 针对接口，需要获取相关的实现类，因为注解是在实现类上的
 *
 * @author Jie, Super Idol lv, Super Idol peng
 * @date 2019-01-07
 */
@Slf4j
@SuppressWarnings("unused")
public class SpringContextHolder implements ApplicationContextAware, DisposableBean {

    //    Spring应用上下文环境
    private static ApplicationContext applicationContext = null;
    private static DefaultListableBeanFactory defaultListableBeanFactory = null;
    private static final List<CallBack> CALL_BACKS = new ArrayList<>();
    private static boolean addCallback = true;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        if (!Objects.isNull(SpringContextHolder.applicationContext))
            log.warn("SpringContextHolder中的ApplicationContext被覆盖, 原有ApplicationContext为: {}", SpringContextHolder.applicationContext);

        SpringContextHolder.applicationContext = applicationContext;
        if (addCallback) {
            for (var callBack : SpringContextHolder.CALL_BACKS)
                callBack.executor();
            // 执行完成后，记得情况释放掉引用，避免因为此处的引用导致该被GC时无法被GC
            CALL_BACKS.clear();
        }
        SpringContextHolder.addCallback = false; // 这个只在启动后执行一下，后面就没必要了。CALL_BACKS里存的就是预先放进去，要在初始化完成后执行的任务。

        defaultListableBeanFactory = (DefaultListableBeanFactory) applicationContext.getAutowireCapableBeanFactory();
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
     * 这个可以说也是在启动后，执行些逻辑，原来实现ApplicationListener做的事情，应该可以通过这个来实现。
     *
     * @param callBack 回调函数
     */
    public static synchronized void addCallBacks(CallBack callBack) {
        if (addCallback) {
            SpringContextHolder.CALL_BACKS.add(callBack);
        } else {
            log.warn("CallBack：{} 容器已启动完毕，已无法添加！立即执行", callBack.getCallBackName());
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
        if (StringUtils.isBlank(beanName)) {
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
     * 根据Class获取所有该类型的Bean，可用于获取类的所有子类、接口的所有实现类
     *
     * @param clazz 类型
     * @return java.util.Map key为beanName，value为beanInstance
     * @date 2021/11/23 11:48 上午
     */
    public static <T> Map<String, T> getBeansOfType(Class<T> clazz) {
        return applicationContext.getBeansOfType(clazz);
    }

    /**
     * 检查ApplicationContext中是否包含
     *
     * @param beanName /
     * @return boolean
     * @date 2021/11/23 9:33 上午
     */
    public static boolean containsBean(String beanName) {
        return applicationContext.containsBean(beanName);
    }

    /**
     * 判断bean是否为单例
     *
     * @param name /
     * @return boolean
     * @date 2021/11/23 9:34 上午
     */
    public static boolean isSingleton(String name) throws NoSuchBeanDefinitionException {
        return applicationContext.isSingleton(name);
    }

    /**
     * 获取bean的类型
     *
     * @param beanName /
     * @return java.lang.Class
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

    // region 加载jar中的class并将bean注入/移出Spring容器

    /**
     * 加入jar包后 动态注册bean到spring容器，包括bean的依赖
     *
     * @param jarAddress jar文件路径为jarAddress， /xxx，若是windows下的路径，下面的jarPath应该改为"file:/" + jarAddress 好像，未验证，当然最稳妥的就是通过file来获取
     */
    public static void registerBeanInJar(String jarAddress) throws IOException, ClassNotFoundException {
        // jar的Url路径为jarPath，jarPath = "file:" + jarAddress。也可以这样：
        var file = new File(jarAddress);
        var jarPath = file.toURI().toURL();
        // 通过线程的上下文类加载器来加载
        // URLClassLoader既可以加载本地文件系统的jar包，也可以加载远程jar包。比如URL jarPath = "https://repo1.maven.org/maven2/com/lwohvye/eladmin-common/3.0.2/eladmin-common-3.0.2.jar" 也可以，只是会比较慢
        // 这种远程执行，印象中在哪里看过
        try (var urlClassLoader = new URLClassLoader(new URL[]{jarPath}, Thread.currentThread().getContextClassLoader())) {
            var classNameSet = readJarFile(jarAddress);
            for (var className : classNameSet) {
                var clazz = urlClassLoader.loadClass(className);
                if (isSpringBeanClass(clazz)) {
                    var beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition(clazz);
                    // 向容器中注册bean，视情况是否允许覆盖
                    defaultListableBeanFactory.registerBeanDefinition(StringUtils.lowerFirstChar(className), beanDefinitionBuilder.getBeanDefinition());
                }
            }
        }
    }

    /**
     * 删除jar包时 需要在spring容器删除注入
     */
    public static void deleteBeanInJar(String jarAddress) throws IOException, ClassNotFoundException {
        try (var urlClassLoader = new URLClassLoader(new URL[]{new URL("file:" + jarAddress)}, Thread.currentThread().getContextClassLoader())) {
            var classNameSet = readJarFile(jarAddress);
            for (var className : classNameSet) {
                // 这里load一下，只是为了拿一下注解，看是不是Spring的Bean
                var clazz = urlClassLoader.loadClass(className);
                if (isSpringBeanClass(clazz)) {
                    // 从容器中删除bean
                    defaultListableBeanFactory.removeBeanDefinition(StringUtils.lowerFirstChar(className));
                }
            }
        }
    }

    /**
     * 读取jar包中所有类文件
     */
    public static Set<String> readJarFile(String jarAddress) throws IOException {
        var classNameSet = new HashSet<String>();
        try (var jarFile = new JarFile(jarAddress)) {
            var entries = jarFile.entries();//遍历整个jar文件
            while (entries.hasMoreElements()) {
                var jarEntry = entries.nextElement();
                var name = jarEntry.getName();
                // module-info.java和package-info.java要忽略的
                if (name.endsWith(".class") && !(name.contains("module-info") || name.contains("package-info"))) {
                    var className = name.replace(".class", "").replace("/", ".");
                    classNameSet.add(className);
                }
            }
        }
        return classNameSet;
    }

    /**
     * 方法描述 判断class对象是否带有spring的注解
     */
    public static boolean isSpringBeanClass(Class<?> cla) {
        if (cla == null) {
            return false;
        }
        //是否是接口
        if (cla.isInterface()) {
            return false;
        }
        //是否是抽象类
        if (Modifier.isAbstract(cla.getModifiers())) {
            return false;
        }
        return cla.getAnnotation(Component.class) != null || cla.getAnnotation(Repository.class) != null || cla.getAnnotation(Service.class) != null;
    }
    // endregion
}
