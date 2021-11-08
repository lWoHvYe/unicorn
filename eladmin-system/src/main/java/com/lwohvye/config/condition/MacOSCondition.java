package com.lwohvye.config.condition;

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
