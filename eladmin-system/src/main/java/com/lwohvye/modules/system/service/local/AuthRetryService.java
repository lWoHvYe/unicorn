package com.lwohvye.modules.system.service.local;

import cn.hutool.core.util.RandomUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class AuthRetryService {

    /**
     * @description
     * @EnableRetry – 表示开启重试机制
     * @Retryable – 表示这个方法需要重试，它有很丰富的参数，可以满足你对重试的需求
     * SimpleRetryPolicy 默认最多重试3次
     * TimeoutRetryPolicy 默认在1秒内失败都会重试
     * ExpressionRetryPolicy 符合表达式就会重试
     * CircuitBreakerRetryPolicy 增加了熔断的机制，如果不在熔断状态，则允许重试
     * CompositeRetryPolicy 可以组合多个重试策略
     * NeverRetryPolicy 从不重试（也是一种重试策略哈）
     * AlwaysRetryPolicy 总是重试
     * @Backoff – 表示重试中的退避策略
     * FixedBackOffPolicy 默认固定延迟1秒后执行下一次重试
     * ExponentialBackOffPolicy 指数递增延迟执行重试，默认初始0.1秒，系数是2，那么下次延迟0.2秒，再下次就是延迟0.4秒，如此类推，最大30秒。
     * ExponentialRandomBackOffPolicy 在上面那个策略上增加随机性
     * UniformRandomBackOffPolicy 这个跟上面的区别就是，上面的延迟会不停递增，这个只会在固定的区间随机
     * StatelessBackOffPolicy 这个说明是无状态的，所谓无状态就是对上次的退避无感知
     * @Recover – 兜底方法，即多次重试后还是失败就会执行这个方法
     * <p>
     * Spring-Retry 的功能丰富在于其重试策略和退避策略，还有兜底，监听器等操作。
     * @author Hongyan Wang
     * @date 2021/4/23 1:13 下午
     */
    @Retryable(value = IllegalAccessException.class, maxAttempts = 5,
            backoff = @Backoff(value = 1500, maxDelay = 100000, multiplier = 1.2))
    public void service() throws IllegalAccessException {
        System.out.println("service method...");
        if (RandomUtil.randomBoolean())
            throw new IllegalAccessException("manual exception");
    }

    @Recover
    public void recover(IllegalAccessException e) {
        System.out.println("service retry after Recover => " + e.getMessage());
    }
}
