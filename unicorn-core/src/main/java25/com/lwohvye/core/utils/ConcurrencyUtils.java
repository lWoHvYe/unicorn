/*
 *    Copyright (c) 2025-2026.  lWoHvYe(Hongyan Wang)
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

import com.lwohvye.core.exception.UtilsException;
import lombok.experimental.UtilityClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.StructuredTaskScope.Subtask;
import java.util.concurrent.StructuredTaskScope.Joiner;

/**
 * This class provides utility methods for handling concurrency and executing tasks in a structured manner.
 *
 * @since 25
 */
@UtilityClass
public class ConcurrencyUtils extends UnicornAbstractThreadUtils {

    //    private static final StableValue<Logger> log = StableValue.of();
    private static final Supplier<Logger> log =
            StableValue.supplier(() -> LoggerFactory.getLogger(ConcurrencyUtils.class));

    /**
     * Basic flow : execute tasks, the result as the input of composeResult, the previous res as the input of eventual
     *
     * @param composeResult consume the task res
     * @param eventual      finally execute, consume the res of  composeResult
     * @param tasks         tasks wtd
     */
    public static <T, U> void structuredExecute(Function<List<T>, U> composeResult, Consumer<U> eventual, Callable<T>... tasks) {
        try (var scope = StructuredTaskScope.open()) { // 使用open默认就是所有的都需要成功
            List<Subtask<T>> subtasks = null;
            if (Objects.nonNull(tasks))
                subtasks = Arrays.stream(tasks).map(scope::fork).toList();
            scope.join();         // Join subtasks, propagating exceptions

            // Both subtasks have succeeded, so compose their results
            U results = null;
            if (Objects.nonNull(composeResult))
                results = composeResult.apply(Objects.nonNull(subtasks) ?
                        subtasks.stream().map(Subtask::get).filter(Objects::nonNull).toList() : Collections.emptyList());
            if (Objects.nonNull(eventual))
                eventual.accept(results);
        } catch (StructuredTaskScope.FailedException | StructuredTaskScope.TimeoutException e) {
            log.get().info("error occurred in 25 Callable util {}", e.getMessage());
            if (e.getCause() instanceof RuntimeException re)
                throw re;
            throw new UtilsException(e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    // Returns a Callable object that, when called, runs the given task and returns null.
    // var callable = Executors.callable(runnable) // Runnable 2 Callable
    // A FutureTask can be used to wrap a Callable or Runnable object. Because FutureTask implements Runnable, a FutureTask can be submitted to an Executor for execution.
    // var futureTask = new FutureTask<T>(Callable/Runnable) // Callable/Runnable 2 Runnable/Future
    public static void structuredExecute(Runnable eventual, Runnable... tasks) {
        try (var scope = StructuredTaskScope.open(Joiner.<Void>allSuccessfulOrThrow(),
                cf -> cf.withName("STS-JUC"))) {
            if (Objects.nonNull(tasks))
                Arrays.stream(tasks).forEach(scope::fork);
            scope.join();         // Join subtasks, propagating exceptions

            // Here, both forks have succeeded, so compose their results
            if (Objects.nonNull(eventual))
                eventual.run();
        } catch (StructuredTaskScope.FailedException | StructuredTaskScope.TimeoutException e) {
            log.get().info("error occurred in 25 Runnable util {}", e.getMessage());
            if (e.getCause() instanceof RuntimeException re)
                throw re;
            throw new UtilsException(e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
