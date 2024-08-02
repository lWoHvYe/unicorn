/*
 *    Copyright (c) 2023-2024.  lWoHvYe(Hongyan Wang)
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

package com.unicorn;

import org.junit.jupiter.api.Test;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.LockSupport;

public class CustomParallelismTest {

    // Global configure ForkJoinPool
    // -Djava.util.concurrent.ForkJoinPool.common.parallelism=8

    @Test
    void testCustomThreadPool() {
        var largeDataSet = new ArrayList<>();
        largeDataSet.add("1");
        largeDataSet.add(2);

        // ExecutorService在Java 21时 implement AutoCloseable，所以可以用 try-with-resource，在Java 17时还是不行的
        // 这里隐式调用shutdown()，需注意调用后只是不再接收新的task，已经提交(至少是获得了thread)的task是继续运行的、并且这些task可以进一步创建子线程，并不影响
        // Possibly initiates an orderly shutdown in which previously submitted tasks are executed, but no new tasks will be accepted.
        // Invocation has no effect on execution state if this is the commonPool(), and no additional effect if already shut down.
        // Tasks that are in the process of being submitted concurrently during the course of this method may or may not be rejected.
        // 并且shutdown()的call并不会block住等待创建的thread结束
        // 所以可能的效果是，main创建了pool-A，提交task_B、task_C，然后pool-A.shutdown()，此时若B、C还在运行，则会继续运行，main也会继续运行并不会等待B、C结束，并且B、C可以继续创建child_thread，理论上只要main不结束，B、C可以一直run
        try (var customThreadPool = new ForkJoinPool(16)) {
            customThreadPool.submit(() ->
                    largeDataSet.parallelStream().forEach(System.out::println));
        }
    }

    @Retryable(retryFor = RuntimeException.class, backoff = @Backoff(value = 1500, maxDelay = 100000, multiplier = 1.2, random = true))
    public String retrySample() {
        // 以下不能确定，当下面配合@Retryable和@Recover 使用时，当future中出现Exception时，wftAll理论上会立即接收到，但似乎不会立即throw并触发retry，而是等所有future都结束后才throw Exception，然后wait指定的时间再触发retry
        var allFuture = new ArrayList<CompletableFuture<String>>();
        try (var pool = new ForkJoinPool(8)) {
            for (int i = 0; i < 10; i++) {
                int finalI = i;
                // async with given pool
                var future = CompletableFuture.supplyAsync(() -> {
                    LockSupport.parkNanos(TimeUnit.SECONDS.toNanos(1L));
                    return "Done-" + finalI;
                }, pool);
                // exception handle
                future = future.handle((result, e) -> {
                    if (e != null) {
                        throw new RuntimeException(e.getMessage(), e);
                    }
                    return result;
                });
                allFuture.add(future);
            }
            var wtfAll = CompletableFuture.allOf(allFuture.toArray(new CompletableFuture[0]));
            wtfAll.join();
            System.out.println("all future completed");
        }
        return "2024";
    }

    @Recover
    public String retrySampleFallback(RuntimeException e) {
        System.out.println("Exception: " + e);
        return "Default";
    }
}
