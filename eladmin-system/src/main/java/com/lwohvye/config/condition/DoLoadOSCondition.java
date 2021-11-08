package com.lwohvye.config.condition;

import cn.hutool.core.util.RandomUtil;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.type.AnnotatedTypeMetadata;

public class DoLoadOSCondition implements Condition {
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        return RandomUtil.randomBoolean();
    }
}
