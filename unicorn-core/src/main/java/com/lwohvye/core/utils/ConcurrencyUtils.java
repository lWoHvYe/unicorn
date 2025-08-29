/*
 *    Copyright (c) 2022-2025.  lWoHvYe(Hongyan Wang)
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
     * Basic flow : execute tasks, the result as the input of composeResult, the previous res as the input of eventual
     *
     * @param composeResult consume the task res
     * @param eventual      finally execute, consume the res of  composeResult
     * @param tasks         tasks wtd
     */
    public static <T, U> void structuredExecute(Function<List<T>, U> composeResult, Consumer<U> eventual, Callable<T>... tasks) {
        log.warn("In Java 17 Source");
        List<CompletableFuture<T>> futures = null;
        if (Objects.nonNull(tasks)) {
            futures = Arrays.stream(tasks).map(task -> CompletableFuture.supplyAsync(() -> {
                try {
                    return task.call();
                } catch (Exception e) {
                    throw new UtilsException(e.getMessage());
                }
            }, TASK_EXECUTOR)).toList();
            var allCF = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
            // whenComplete 只是一个处理完成状态的方法，它不会吞掉异常，也不会改变 CompletableFuture 的状态。
            allCF.whenComplete((voidResult, throwable) -> {
                if (throwable == null) {
                    log.info("All tasks completed successfully");
                } else {
                    log.warn("Exception occurred when execute task: {} ", throwable.getMessage());
                }
            });
            allCF.join(); // This will still throw an exception if any of the futures failed
        }
        U results = null;
        if (Objects.nonNull(composeResult))
            results = composeResult.apply(Objects.nonNull(futures) ?
                    futures.stream().map(CompletableFuture::join).filter(Objects::nonNull).toList() : Collections.emptyList());
        if (Objects.nonNull(eventual))
            eventual.accept(results);

    }

    // Returns a Callable object that, when called, runs the given task and returns null.
    // var callable = Executors.callable(runnable) // Runnable 2 Callable
    // A FutureTask can be used to wrap a Callable or Runnable object. Because FutureTask implements Runnable, a FutureTask can be submitted to an Executor for execution.
    // var futureTask = new FutureTask<T>(Callable/Runnable) // Callable/Runnable 2 Runnable/Future
    public static void structuredExecute(Runnable eventual, Runnable... tasks) {
        log.warn("In Java 17 Source");
        if (Objects.nonNull(tasks)) {
            var futureTasks = Arrays.stream(tasks).map(task -> CompletableFuture.runAsync(task, TASK_EXECUTOR)).toList();
            CompletableFuture.allOf(futureTasks.toArray(new CompletableFuture[0])).join();
        }
        // Here, both forks have succeeded, so compose their results
        if (Objects.nonNull(eventual))
            eventual.run();
    }
}
