/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

import jdk.incubator.concurrent.StructuredTaskScope;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.function.Consumer;
import java.util.function.Function;

public final class ConcurrencyUtils {

    ConcurrencyUtils() {
    }

    static final ThreadFactory virtualFactory = Thread.ofVirtual().name("Virtual-Concurrency").factory();

    /**
     * Basic flow : execute tasks, the result as the input of composeResult, the previous res as the input of eventual
     *
     * @param composeResult consume the task res
     * @param eventual      finally execute, consume the res of  composeResult
     * @param tasks         tasks wtd
     * @date 2022/9/22 8:26 PM
     */
    public static void structuredExecute(Function<List<?>, ?> composeResult, Consumer<Object> eventual, Callable<?>... tasks) {
        //  The default virtual thread factory can work without enable preview.
        //  It uses reflection to allow this class be compiled in an incubator module without also enabling preview features.
        try (var scope = new StructuredTaskScope.ShutdownOnFailure("STS-JUC", virtualFactory)) {
            List<? extends Future<?>> futures = null;
            if (Objects.nonNull(tasks))
                futures = Arrays.stream(tasks).map(scope::fork).toList();

            scope.join();           // Join both forks
            scope.throwIfFailed();  // ... and propagate errors

            // Here, both forks have succeeded, so compose their results
            Object results = null;
            if (Objects.nonNull(composeResult))
                results = composeResult.apply(Objects.nonNull(futures) ? futures.stream().map(Future::resultNow).toList() : Collections.emptyList());
            if (Objects.nonNull(eventual))
                eventual.accept(results);
        } catch (ExecutionException e) {
            if (e.getCause() instanceof RuntimeException re)
                throw re;
            throw new RuntimeException(e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
