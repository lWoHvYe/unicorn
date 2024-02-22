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
package com.lwohvye.sys.modules.system.service.local;

import cn.hutool.core.util.RandomUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import java.io.IOException;

@Slf4j
@Service
public class AuthRetryService {

    // https://github.com/spring-projects/spring-retry
    // 使用RetryTemplate是另一种重试的配置方式，较注解的方式复杂，但更为灵活。一般用注解但方式。
    static {
        RetryTemplate.builder()
                .maxAttempts(10)
                // 指数递增延迟
                .exponentialBackoff(100, 2, 10000)
                .retryOn(IOException.class)
                .traversingCauses()
                .build();

        RetryTemplate.builder()
                // 固定延迟
                .fixedBackoff(10)
                .withTimeout(3000)
                .build();

        RetryTemplate.builder()
                .infiniteRetry()
                .retryOn(IOException.class)
                .uniformRandomBackoff(1000, 3000)
                .build();

        RetryTemplate template = RetryTemplate.builder()
                .maxAttempts(3)
                .fixedBackoff(1000)
                .retryOn(RestClientException.class)
                .build();

        template.execute(ctx -> {
            // ... do something
            return null;
        });
    }

    /**
     * -----------------------------------------------------------------------------------------
     *
     * @EnableRetry – 表示开启重试机制
     * 对于@EnableRetry中的proxyTargetClass参数，是控制是否使用Cglib动态代理，默认的情况下为false，表示使用Jdk动态代理。
     * ------------------------------------------------------------------------------------------------------
     * @Retryable – 表示这个方法需要重试，它有很丰富的参数，可以满足你对重试的需求
     * recover：指定兜底/补偿的方法名。如果不指定，默认对照 @Recover 标识的，第一入参为重试异常，其余入参和出参一致的方法；
     * interceptor：指定方法切面 bean， org.aopalliance.intercept.MethodInterceptor 实现类
     * retryFor / include：两者用途一致，指出哪些类型需要重试；
     * exclude：和 include 相反，指出哪些异常不需要重试；
     * label：可以指定唯一标签，用于统计；
     * stateful：默认false。重试是否是有状态的；
     * maxAttempts：最大的重试次数；
     * backoff：指定 @Backoff ，回退策略；
     * listeners：指定 org.springframework.retry.RetryListener 实现 bean。
     * 重试策略
     * SimpleRetryPolicy 默认最多重试3次
     * TimeoutRetryPolicy 默认在1秒内失败都会重试
     * ExpressionRetryPolicy 符合表达式就会重试
     * CircuitBreakerRetryPolicy 增加了熔断的机制，如果不在熔断状态，则允许重试
     * CompositeRetryPolicy 可以组合多个重试策略
     * NeverRetryPolicy 从不重试（也是一种重试策略哈）
     * AlwaysRetryPolicy 总是重试
     * ------------------------------------------------------------------------------------------------------
     * @Backoff – 表示重试中的退避策略
     * value / delay：两者都标识延迟时间，为 0则对应 NoBackOffPolicy 策略。
     * maxDelay：最大延迟时间
     * multiplier：递增乘数
     * random：递增乘数是否随机
     * 退避策略
     * FixedBackOffPolicy 默认固定延迟1秒后执行下一次重试
     * ExponentialBackOffPolicy 指数递增延迟执行重试，默认初始0.1秒，系数是2，那么下次延迟0.2秒，再下次就是延迟0.4秒，如此类推，最大30秒。
     * ExponentialRandomBackOffPolicy 在上面那个策略上增加随机性
     * UniformRandomBackOffPolicy 这个跟上面的区别就是，上面的延迟会不停递增，这个只会在固定的区间随机
     * StatelessBackOffPolicy 这个说明是无状态的，所谓无状态就是对上次的退避无感知
     * ------------------------------------------------------------------------------------------------------
     * @Recover – 兜底方法，即多次重试后还是失败就会执行这个方法
     * <p>
     * Spring-Retry 的功能丰富在于其重试策略和退避策略，还有兜底，监听器等操作。
     * 由于@Retryable注解是通过切面实现的，因此要避免@Retryable 注解的方法的调用方和被调用方处于同一个类中，这样会使@Retryable 注解失效
     * ------------------------------------------------------------------------------------------------------
     * @Retryable标记的方法，不必是接口的实现，但调用方需在另一个类中
     * @Recover标记的方法需与@Retryable标记的方法在同一类中
     * @date 2021/4/23 1:13 下午
     */
    @Retryable(retryFor = IllegalAccessException.class, maxAttempts = 5,
            backoff = @Backoff(value = 1500, maxDelay = 100000, multiplier = 1.2, random = true))
    public void retryService(String str) throws IllegalAccessException {
        log.info("service method...      start");
        if (RandomUtil.randomBoolean())
            throw new IllegalAccessException("manual exception");
        log.info("service method...      end");
    }

    /**
     * 在@Retryable多次重试失败后，调用该方法。
     * 要触发@Recover标记的方法，@Retryable标记的方法不能有返回值，只能是void才能触发。
     * 被@Recover标记的方法的第一入参要与发生的异常一至，才会被调用
     */
    @Recover
    public void recover(IllegalAccessException e) {
        log.error("service retry after Recover => {}", e.getMessage());
    }
}
