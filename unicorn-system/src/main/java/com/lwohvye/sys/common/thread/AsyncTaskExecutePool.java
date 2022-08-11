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
package com.lwohvye.sys.common.thread;

import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * 异步任务线程池装配类。当前用于@Async。异步执行
 * 自定义线程池有如下模式：
 * 重新实现接口AsyncConfigurer
 * 继承AsyncConfigurerSupport
 * 配置由自定义的TaskExecutor替代内置的任务执行器
 *
 * @author https://juejin.im/entry/5abb8f6951882555677e9da2
 * @date 2019年10月31日15:06:18
 */
@Slf4j
@Configuration
public class AsyncTaskExecutePool implements AsyncConfigurer {

    /**
     * 注入配置类
     */
    private final AsyncTaskProperties config;

    public AsyncTaskExecutePool(AsyncTaskProperties config) {
        this.config = config;
    }

    @Override
    public Executor getAsyncExecutor() {
        // Spring 默认配置是核心线程数大小为1，最大线程容量大小不受限制，队列容量也不受限制。
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        // 核心线程数，即同时运行的最大线程数
        executor.setCorePoolSize(config.getCorePoolSize());
        // 最大线程数，当队列满时，才会根据该参数创建线程，且保证最终线程数不超过该值，对于无界队列，该参数用不到
        executor.setMaxPoolSize(config.getMaxPoolSize());
        //队列容量。队列默认是LinkedBlockingQueue
        executor.setQueueCapacity(config.getQueueCapacity());
        //活跃时间
        executor.setKeepAliveSeconds(config.getKeepAliveSeconds());
        //线程名字前缀
        executor.setThreadNamePrefix("lWoHvYe-async-");
        // setRejectedExecutionHandler：当pool已经达到max size的时候，如何处理新任务
        // CallerRunsPolicy：不在新线程中执行任务，而是由调用者所在的线程来执行
        // 考虑到承载量等，用默认的拒绝并抛异常更好些。下面这块可以注释掉
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.AbortPolicy());
        // 等待所有任务都完成再继续销毁其他的Bean
        executor.setWaitForTasksToCompleteOnShutdown(config.isWaitForTasksToComplete());
        // 线程池中任务的等待时间，如果超过这个时候还没有销毁就强制销毁，以确保应用最后能够被关闭，而不是阻塞住
        executor.setAwaitTerminationSeconds(config.getAwaitTerminationSeconds());
        // 执行初始化
        executor.initialize();
        return executor;
    }

    /**
     * 这里定义了异步执行异常的处理。
     * 线程池执行异常处理方式有四种：
     * 自行try-cache、
     * 返回Future调用其get()会阻塞的返回结果，若执行出错会抛出异常
     * 在new Thread时设置UncaughtExceptionHandler、
     * 重写ThreadPoolExecutor的afterExecute方法，处理传递的异常引用
     * 在Java 8中，CompletableFuture，这种同步/异步编程，也支持定义异常处理等
     * 针对异步这个，一般用Spring的ThreadPoolTaskExecutor，可以通过下面的方式定义异常处理
     *
     * @return org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler
     * @date 2022/4/27 6:33 PM
     */
    @Override
    public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
        return (throwable, method, objects) -> {
            log.error("====" + throwable.getMessage() + "====", throwable);
            log.error("exception method:" + method.getName());
        };
    }
}
