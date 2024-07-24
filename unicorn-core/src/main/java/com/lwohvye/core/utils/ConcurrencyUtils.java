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
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

@Slf4j
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
        log.warn("In Java 17 Source");
        List<? extends CompletableFuture<?>> futures = null;
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
        Object results = null;
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

    /*
        var i = c.incrementAndGet();
        ConcurrencyUtils.threadLocal.set(String.valueOf(i));
        threadPoolExecutor.execute(ConcurrencyUtils.withTLTP(() -> {
            var s = ConcurrencyUtils.threadLocal.get(); // 这里获得的就是Target val
            log.info((String) s);
        }));
        threadLocal.remove();
     */

    /*
        var voidCompletableFuture = CompletableFuture.runAsync(ConcurrencyUtils.withTLTP(() -> {
            var s = ConcurrencyUtils.threadLocal.get();
            log.info((String) s);
        }));
        var unused = voidCompletableFuture.get();

        var stringCompletableFuture = CompletableFuture.supplyAsync(ConcurrencyUtils.withTLTP(() -> {
            var s = ConcurrencyUtils.threadLocal.get();
            log.info((String) s);
            return String.valueOf(s);
        }));
        var s = stringCompletableFuture.get();
     */

    public static Runnable withTLTP(Runnable runnable) {
        var sharedVar = ConcurrencyUtils.threadLocal.get(); // 在parent thread中执行
        return () -> { // sharedVar的传递还不清楚，但已知因为是非基本类型，所以传递的引用
            ConcurrencyUtils.threadLocal.set(sharedVar); // 在sub thread中执行
            runnable.run(); // runnable在调用 run()的线程执行，当前是 sub thread。
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
