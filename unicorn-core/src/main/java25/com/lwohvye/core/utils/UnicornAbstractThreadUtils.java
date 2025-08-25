/*
 *    Copyright (c) 2023-2025.  lWoHvYe(Hongyan Wang)
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

import module java.base;

import io.micrometer.context.ContextExecutorService;
import io.micrometer.context.ContextSnapshotFactory;
import lombok.extern.slf4j.Slf4j;

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
    public static ExecutorService wrap(ExecutorService executor) {
        return ContextExecutorService.wrap(executor, () -> ContextSnapshotFactory.builder().build().captureAll());
    }
}
