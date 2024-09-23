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
package com.lwohvye.log.aspect;

import com.lwohvye.log.domain.BzLog;
import com.lwohvye.log.service.IBzLogService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.lwohvye.core.utils.RequestHolder;
import com.lwohvye.core.utils.SecurityUtils;
import com.lwohvye.core.utils.StringUtils;
import com.lwohvye.core.utils.ThrowableUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.time.Instant;


/**
 * @author Zheng Jie
 * @date 2018-11-24
 */
@Slf4j
@Aspect
@Component
@RequiredArgsConstructor
public class OprLogAspect {

    private final IBzLogService oprLogService;

    static final ScopedValue<Instant> SCOPED_VALUE = ScopedValue.newInstance();

    /**
     * 配置切入点
     */
    @Pointcut("@annotation(com.lwohvye.core.annotation.log.OprLog)")
    public void logPointcut() {
        // 该方法无方法体,主要为了让同类中其他方法使用此切入点
    }

    /**
     * 配置环绕通知,使用在方法logPointcut()上注册的切入点
     *
     * @param joinPoint join point for advice
     */
    @Around("logPointcut()")
    public Object logAround(ProceedingJoinPoint joinPoint) throws Throwable {
        return ScopedValue.where(SCOPED_VALUE, Instant.now()).call(() -> {
            Object result;
            try {
                result = joinPoint.proceed();
            } catch (Throwable e) {
                throw new RuntimeException(e.getMessage());
            }
            var bzLog = new BzLog("INFO", Duration.between(SCOPED_VALUE.get(), Instant.now()).toMillis());
            var request = RequestHolder.getHttpServletRequest();
            // TODO: 2022/2/14 保持日志这块，有循环依赖导致的栈溢出问题，待解决
            // TODO: 2022/2/28 栈溢出应该有别的条件。人员状态的修改，未出现该问题
            oprLogService.save(getUsername(), StringUtils.getBrowser(request), StringUtils.getIp(request), joinPoint, bzLog);
            return result;
        });
    }

    /**
     * 配置异常通知
     *
     * @param joinPoint join point for advice
     * @param e         exception
     */
    @AfterThrowing(pointcut = "logPointcut()", throwing = "e")
    public void logAfterThrowing(JoinPoint joinPoint, Throwable e) {
        if (!SCOPED_VALUE.isBound()) return;
        var bzLog = new BzLog("ERROR", Duration.between(SCOPED_VALUE.get(), Instant.now()).toMillis());
        bzLog.setExceptionDetail(ThrowableUtils.getStackTrace(e).getBytes());
        var request = RequestHolder.getHttpServletRequest();
        oprLogService.save(getUsername(), StringUtils.getBrowser(request), StringUtils.getIp(request), (ProceedingJoinPoint) joinPoint, bzLog);
    }

    public String getUsername() {
        try {
            return SecurityUtils.getCurrentUsername();
        } catch (Exception e) {
            return "";
        }
    }
}
