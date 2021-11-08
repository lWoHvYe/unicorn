package com.lwohvye.config.condition;

import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.type.AnnotatedTypeMetadata;

public class WindowsCondition implements Condition {
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        //包含Windows则说明是windows系统，返回true
        return context.getEnvironment().getProperty("os.name", "UNIX").contains("Windows");
    }
}
