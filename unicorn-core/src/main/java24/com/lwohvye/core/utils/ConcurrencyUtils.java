/*
 *    Copyright (c) 2025.  lWoHvYe(Hongyan Wang)
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
import org.slf4j.MDC;

import java.util.concurrent.StructuredTaskScope.Subtask;

/**
 * This class provides utility methods for handling concurrency and executing tasks in a structured manner.
 */
@UtilityClass
public class ConcurrencyUtils extends UnicornAbstractThreadUtils {

    /**
     * Basic flow : execute tasks, the result as the input of composeResult, the previous res as the input of eventual
     *
     * @param composeResult consume the task res
     * @param eventual      finally execute, consume the res of  composeResult
     * @param tasks         tasks wtd
     */
    public static void structuredExecute(Function<List<?>, ?> composeResult, Consumer<Object> eventual, Callable<?>... tasks) {
        try (var scope = new StructuredTaskScope.ShutdownOnFailure("STS-JUC", virtualFactory)) {
            List<? extends Subtask<?>> subtasks = null;
            if (Objects.nonNull(tasks))
                subtasks = Arrays.stream(tasks).map(scope::fork).toList();

            scope.join()           // Join both forks
                    .throwIfFailed();  // ... and propagate errors

            // Here, both forks have succeeded, so compose their results
            Object results = null;
            if (Objects.nonNull(composeResult))
                results = composeResult.apply(Objects.nonNull(subtasks) ?
                        subtasks.stream().map(Subtask::get).filter(Objects::nonNull).toList() : Collections.emptyList());
            if (Objects.nonNull(eventual))
                eventual.accept(results);
        } catch (ExecutionException e) {
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
        try (var scope = new StructuredTaskScope.ShutdownOnFailure("STS-JUC", virtualFactory)) {
            if (Objects.nonNull(tasks))
                Arrays.stream(tasks).forEach(runnable -> scope.fork(Executors.callable(runnable)));

            scope.join()           // Join both forks
                    .throwIfFailed();  // ... and propagate errors

            // Here, both forks have succeeded, so compose their results
            if (Objects.nonNull(eventual))
                eventual.run();
        } catch (ExecutionException e) {
            if (e.getCause() instanceof RuntimeException re)
                throw re;
            throw new UtilsException(e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public static Runnable decorateMdc(Runnable runnable) {
        var mdc = MDC.getCopyOfContextMap();
        return () -> {
            MDC.setContextMap(mdc);
            runnable.run();
        };
    }

    public static <U> Supplier<U> decorateMdc(Supplier<U> supplier) {
        var mdc = MDC.getCopyOfContextMap();
        return () -> {
            MDC.setContextMap(mdc);
            return supplier.get();
        };
    }
}
