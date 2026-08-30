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
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestAttributesThreadLocalAccessor;
import org.springframework.web.context.request.RequestContextHolder;

import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.function.Supplier;

/**
 * Represents a utility class for handling threads in a virtualized environment.
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

    private static final ContextRegistry limitedRegistry = new ContextRegistry()
            .registerThreadLocalAccessor(new ObservationThreadLocalAccessor())
            .registerThreadLocalAccessor(new RequestAttributesThreadLocalAccessor());

    private static final ContextSnapshotFactory selectiveFactory = ContextSnapshotFactory.builder()
            .contextRegistry(limitedRegistry)
            .build();

    public static ExecutorService wrap(ExecutorService executor) {
        return ContextExecutorService.wrap(executor, () -> selectiveFactory.captureAll());
    }

    public static Runnable decorateObservation(Runnable runnable) {
        var currentObservation = SpringContextHolder.getBean(ObservationRegistry.class).getCurrentObservation();
        return currentObservation != null ? currentObservation.wrap(runnable) : runnable;
    }

    public static <U> Supplier<U> decorateObservation(Supplier<U> supplier) {
        var currentObservation = SpringContextHolder.getBean(ObservationRegistry.class).getCurrentObservation();
        return currentObservation != null ? currentObservation.wrap(supplier) : supplier;
    }

    public static Runnable decorateMdc(Runnable runnable) {
        Map<String, String> capturedMdc = MDC.getCopyOfContextMap();
        return () -> {
            Map<String, String> previousMdc = MDC.getCopyOfContextMap();
            try {
                restoreMdc(capturedMdc);
                runnable.run();
            } finally {
                restoreMdc(previousMdc);
            }
        };
    }

    public static <U> Supplier<U> decorateMdc(Supplier<U> supplier) {
        Map<String, String> capturedMdc = MDC.getCopyOfContextMap();
        return () -> {
            Map<String, String> previousMdc = MDC.getCopyOfContextMap();
            try {
                restoreMdc(capturedMdc);
                return supplier.get();
            } finally {
                restoreMdc(previousMdc);
            }
        };
    }

    public static Runnable decorateRequest(Runnable runnable) {
        RequestAttributes capturedAttributes = RequestContextHolder.currentRequestAttributes();
        return () -> {
            RequestAttributes previousAttributes = RequestContextHolder.getRequestAttributes();
            try {
                RequestContextHolder.setRequestAttributes(capturedAttributes, false);
                runnable.run();
            } finally {
                RequestContextHolder.setRequestAttributes(previousAttributes, false);
            }
        };
    }

    public static <U> Supplier<U> decorateRequest(Supplier<U> supplier) {
        RequestAttributes capturedAttributes = RequestContextHolder.currentRequestAttributes();
        return () -> {
            RequestAttributes previousAttributes = RequestContextHolder.getRequestAttributes();
            try {
                RequestContextHolder.setRequestAttributes(capturedAttributes, false);
                return supplier.get();
            } finally {
                RequestContextHolder.setRequestAttributes(previousAttributes, false);
            }
        };
    }

    private static void restoreMdc(Map<String, String> contextMap) {
        if (contextMap == null || contextMap.isEmpty()) {
            MDC.clear();
        } else {
            MDC.setContextMap(contextMap);
        }
    }
}
