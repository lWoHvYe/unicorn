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
package com.lwohvye.core.aspect;

import com.lwohvye.core.annotation.Limit;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.RequestHolder;
import com.lwohvye.core.utils.StringUtils;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.redisson.api.RateIntervalUnit;
import org.redisson.api.RateType;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.stereotype.Component;

/**
 * @author /
 */
@Aspect
@Component
@RequiredArgsConstructor
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET) // 在Spring为Web服务时生效
public class LimitAspect {

    private static final Logger logger = LoggerFactory.getLogger(LimitAspect.class);

    private final RedissonClient redissonClient;

    @Pointcut("@annotation(com.lwohvye.core.annotation.Limit)")
    public void pointcut() {
    }

    @Around("pointcut()")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        var request = RequestHolder.getHttpServletRequest();
        var signature = (MethodSignature) joinPoint.getSignature();
        var signatureMethod = signature.getMethod();
        Limit limit = signatureMethod.getAnnotation(Limit.class);
        var limitType = limit.limitType();
        var key = limit.key();
        if (StringUtils.isBlank(key)) {
            if (limitType == LimitType.IP) {
                key = StringUtils.getIp(request);
            } else {
                key = signatureMethod.getName();
            }
        }

        var rateLimiter = redissonClient.getRateLimiter(key);

        rateLimiter.trySetRate(RateType.OVERALL, limit.count(), limit.period(), RateIntervalUnit.SECONDS);
        if (rateLimiter.tryAcquire(1)) {
            logger.info("key {}，availablePermits {}", limit.key(), rateLimiter.availablePermits());
            return joinPoint.proceed();
        } else {
            throw new BadRequestException(limit.name() + " -> 访问次数受限制");
        }
    }
}
