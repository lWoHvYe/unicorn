/*
 *    Copyright (c) 2022-2026.  lWoHvYe(Hongyan Wang)
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

import com.lwohvye.core.exception.UtilsException;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.function.Function;

@Slf4j
@UtilityClass
public class ConcurrencyUtils extends UnicornAbstractThreadUtils {

    /**
     * Execute all tasks concurrently, compose their results, and optionally consume the composed result.
     *
     * <p>All task failures are propagated and the compose/eventual callbacks are invoked only after every
     * task has completed successfully. A task returning {@code null} keeps its position in the result list.</p>
     *
     * @param composeResult consume the task results
     * @param eventual finally execute, consuming the composed result
     * @param tasks tasks to execute concurrently
     */
    public static <T, U> void structuredExecute(Function<List<T>, U> composeResult, Consumer<U> eventual, Callable<T>... tasks) {
        log.debug("Executing tasks with Java 17 concurrency implementation");
        List<CompletableFuture<T>> futures = tasks == null
                ? Collections.emptyList()
                : Arrays.stream(tasks)
                .map(Objects::requireNonNull)
                .map(task -> CompletableFuture.supplyAsync(() -> {
                    try {
                        return task.call();
                    } catch (Exception e) {
                        throw new UtilsException("Task execution failed", e);
                    }
                }, TASK_EXECUTOR))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        U results = null;
        if (composeResult != null) {
            results = composeResult.apply(futures.isEmpty()
                    ? Collections.emptyList()
                    : futures.stream().map(CompletableFuture::join).toList());
        }
        if (eventual != null) {
            eventual.accept(results);
        }
    }

    /**
     * Execute all tasks concurrently and invoke {@code eventual} after successful completion.
     */
    public static void structuredExecute(Runnable eventual, Runnable... tasks) {
        log.debug("Executing tasks with Java 17 concurrency implementation");
        if (tasks != null) {
            var futures = Arrays.stream(tasks)
                    .map(Objects::requireNonNull)
                    .map(task -> CompletableFuture.runAsync(task, TASK_EXECUTOR))
                    .toList();
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        }
        if (eventual != null) {
            eventual.run();
        }
    }
}
