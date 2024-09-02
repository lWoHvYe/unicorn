/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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
import org.slf4j.MDC;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.concurrent.StructuredTaskScope.Subtask;

/**
 * This class provides utility methods for handling concurrency and executing tasks in a structured manner.
 */
@UtilityClass
public class ConcurrencyUtils extends UnicornAbstractThreadUtils {

    public static final ThreadLocal<Object> threadLocal = new ThreadLocal<>();

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

    // 下面这个，就是解决InheritableThreadLocal 和 ThreadPool一起使用时的问题，使用ThreadLocal 然后自行实现值的传递
    // 因为ITL只在Thread Create时传递，而ThreadPool通常是share的，所以当run CompletableFuture时，ITL会失效，
    // 对此可以在每次run一批Task时 Create New ThreadPool，且避免Thread的复用，因为若复用Thread仍会有该问题,这有悖Pool的部分初衷了
    // 当使用Virtual Threads时，虽然也可以定义ThreadPool,但每次都是New Thread，不会复用，是否还有这个问题，待验证，但用VT时，更推荐用ScopedValue
    public static Runnable withTLTP(Runnable runnable) {
        var sharedVar = ConcurrencyUtils.threadLocal.get();
        return () -> {
            ConcurrencyUtils.threadLocal.set(sharedVar);
            runnable.run();
        };
    }

    public static <U> Supplier<U> withTLTP(Supplier<U> supplier) {
        var sharedVar = ConcurrencyUtils.threadLocal.get();
        return () -> {
            ConcurrencyUtils.threadLocal.set(sharedVar);
            return supplier.get();
        };
    }

    public static <V> Callable<V> withTLTP(Callable<V> callable) {
        var sharedVar = ConcurrencyUtils.threadLocal.get();
        return () -> {
            ConcurrencyUtils.threadLocal.set(sharedVar);
            return callable.call();
        };
    }

    // 下面这俩采用类似的思想
    public static Runnable withMdc(Runnable runnable) {
        var mdc = MDC.getCopyOfContextMap();
        return () -> {
            MDC.setContextMap(mdc);
            runnable.run();
        };
    }

    public static <U> Supplier<U> withMdc(Supplier<U> supplier) {
        var mdc = MDC.getCopyOfContextMap();
        return () -> {
            MDC.setContextMap(mdc);
            return supplier.get();
        };
    }
}
