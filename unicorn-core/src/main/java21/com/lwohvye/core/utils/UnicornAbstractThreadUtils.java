/*
 *    Copyright (c) 2023-2026.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.utils;

import io.micrometer.context.ContextExecutorService;
import io.micrometer.context.ContextRegistry;
import io.micrometer.context.ContextSnapshotFactory;
import io.micrometer.observation.ObservationRegistry;
import io.micrometer.observation.contextpropagation.ObservationThreadLocalAccessor;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.web.context.request.RequestAttributesThreadLocalAccessor;
import org.springframework.web.context.request.RequestContextHolder;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.function.Supplier;

/**
 * Represents a utility class for handling threads in a virtualized environment.
 * This abstract class provides a thread factory and an executor service for executing tasks.
 *
 * @since 21
 */
@Slf4j
public abstract class UnicornAbstractThreadUtils {

    protected UnicornAbstractThreadUtils() {
        log.info("Virtual Thread is Supporting Current JDK Runtime");
    }

    static final ThreadFactory virtualFactory = Thread.ofVirtual().name("Virtual-Concurrency").factory();
    public static final ExecutorService TASK_EXECUTOR = Executors.newThreadPerTaskExecutor(virtualFactory);

    // https://stackoverflow.com/questions/78122797/how-to-propagate-traceid-to-other-threads-in-one-transaction-for-spring-boot-3-x
    // 1. 创建一个只包含特定 Accessor 的 Registry
    static ContextRegistry limitedRegistry = new ContextRegistry()
            .registerThreadLocalAccessor(new ObservationThreadLocalAccessor())         // 只传 Observation
            .registerThreadLocalAccessor(new RequestAttributesThreadLocalAccessor());  // 或自定义字段

    // 2. 使用这个受限的 Registry 创建工厂
    static ContextSnapshotFactory selectiveFactory = ContextSnapshotFactory.builder()
            .contextRegistry(limitedRegistry)
            .build();

    // 使用自定义工厂
    public static ExecutorService wrap(ExecutorService executor) {
        return ContextExecutorService.wrap(executor, () -> selectiveFactory.captureAll());
    }

    public static Runnable decorateObservation(Runnable runnable) {
        // 获取当前 Observation 并包装任务
        var currentObservation = SpringContextHolder.getBean(ObservationRegistry.class).getCurrentObservation();
        if (currentObservation != null) {
            // 包装后的任务在执行时会自动恢复并清理 Trace 上下文
            return currentObservation.wrap(runnable);
        } else {
            return runnable;
        }
    }

    public static <U> Supplier<U> decorateObservation(Supplier<U> supplier) {
        // 获取当前 Observation 并包装任务
        var currentObservation = SpringContextHolder.getBean(ObservationRegistry.class).getCurrentObservation();
        if (currentObservation != null) {
            // 包装后的任务在执行时会自动恢复并清理 Trace 上下文
            return currentObservation.wrap(supplier);
        } else {
            return supplier;
        }
    }

    // 下面这个，就是解决InheritableThreadLocal 和 ThreadPool一起使用时的问题，使用ThreadLocal 然后自行实现值的传递
    // 因为ITL只在Thread Create时传递，而ThreadPool通常是share的，所以当run CompletableFuture时，ITL会失效，
    // 对此可以在每次run一批Task时 Create New ThreadPool，且避免Thread的复用，因为若复用Thread仍会有该问题,这有悖Pool的部分初衷了
    // 当使用Virtual Threads时，虽然也可以定义ThreadPool,但每次都是New Thread，不会复用，是否还有这个问题，待验证，但用VT时，更推荐用ScopedValue
/*
    public static final ThreadLocal<Object> threadLocal = new ThreadLocal<>();

    public static Runnable withTLTP(Runnable runnable) {
        var sharedVar = ConcurrencyUtils.threadLocal.get();
        return () -> {
            ConcurrencyUtils.threadLocal.set(sharedVar);
            runnable.run();
        };
    }*/
//    {
//        // 使用下面这两种方式，可以将traceId等ThreadLocal传到子线程，且ThreadPool的复用不受影响
//        ExecutorService executor = ContextExecutorService.wrap(Executors.newSingleThreadExecutor());
//        var executorService = wrap(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors()));
//          配合下面这个传递custom MDC，另外可以考虑TaskDecorator
//        ContextRegistry.getInstance().registerThreadLocalAccessor("MDC",MDC::getCopyOfContextMap, MDC::setContextMap, MDC::clear);
//    }

    // 下面这俩采用类似的思想
    public static Runnable decorateMdc(Runnable runnable) {
        var mdc = MDC.getCopyOfContextMap();
        return () -> {
            try {
                MDC.setContextMap(mdc);
                runnable.run();
            } finally {
                MDC.clear();
            }
        };
    }

    public static <U> Supplier<U> decorateMdc(Supplier<U> supplier) {
        var mdc = MDC.getCopyOfContextMap();
        return () -> {
            try {
                MDC.setContextMap(mdc);
                return supplier.get();
            } finally {
                MDC.clear();
            }
        };
    }

    public static Runnable decorateRequest(Runnable runnable) {
        var requestAttributes = RequestContextHolder.currentRequestAttributes();
        return () -> {
            try {
                RequestContextHolder.setRequestAttributes(requestAttributes, true);
                runnable.run();
            } finally {
                RequestContextHolder.resetRequestAttributes();
            }
        };
    }

    public static <U> Supplier<U> decorateRequest(Supplier<U> supplier) {
        var requestAttributes = RequestContextHolder.currentRequestAttributes();
        return () -> {
            try {
                RequestContextHolder.setRequestAttributes(requestAttributes, true);
                return supplier.get();
            } finally {
                RequestContextHolder.resetRequestAttributes();
            }
        };
    }
}
