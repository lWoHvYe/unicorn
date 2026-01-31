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
import java.util.function.Supplier;

@Slf4j
public abstract class UnicornAbstractThreadUtils {

    protected UnicornAbstractThreadUtils() {
        log.warn("Virtual Thread only Support Java 21+");
    }

    public static final ExecutorService TASK_EXECUTOR = Executors.newFixedThreadPool(8);

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

    // 将HttpRequest传递到子线程，使用线程池时可以这样做，但也需要考虑性能问题。如果是直接创建子线程就不用这么麻烦，想办法像下面将inheritable设置为true就行
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
