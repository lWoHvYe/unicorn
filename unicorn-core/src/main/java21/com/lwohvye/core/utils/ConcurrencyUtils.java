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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.function.Consumer;
import java.util.function.Function;

import static java.util.concurrent.StructuredTaskScope.Subtask;

/**
 * Utilities for executing tasks with structured concurrency on Java 21+ runtimes.
 */
@UtilityClass
public class ConcurrencyUtils extends UnicornAbstractThreadUtils {

    /**
     * Execute all tasks in a {@link java.util.concurrent.StructuredTaskScope.ShutdownOnFailure}, compose their
     * results, and optionally consume the composed result. If one task fails, sibling tasks are cancelled.
     *
     * @param composeResult consume the task results
     * @param eventual finally execute, consuming the composed result
     * @param tasks tasks to execute concurrently
     */
    public static <T, U> void structuredExecute(Function<List<T>, U> composeResult, Consumer<U> eventual, Callable<T>... tasks) {
        try (var scope = new java.util.concurrent.StructuredTaskScope.ShutdownOnFailure("STS-JUC", virtualFactory)) {
            List<Subtask<T>> subtasks = tasks == null
                    ? Collections.emptyList()
                    : Arrays.stream(tasks)
                    .map(Objects::requireNonNull)
                    .map(scope::fork)
                    .toList();

            scope.join().throwIfFailed();

            U results = null;
            if (composeResult != null) {
                results = composeResult.apply(subtasks.stream().map(Subtask::get).toList());
            }
            if (eventual != null) {
                eventual.accept(results);
            }
        } catch (ExecutionException e) {
            rethrowTaskFailure(e.getCause());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new UtilsException("Structured task execution was interrupted", e);
        }
    }

    /**
     * Execute all tasks concurrently and invoke {@code eventual} after successful completion.
     */
    public static void structuredExecute(Runnable eventual, Runnable... tasks) {
        try (var scope = new java.util.concurrent.StructuredTaskScope.ShutdownOnFailure("STS-JUC", virtualFactory)) {
            if (tasks != null) {
                Arrays.stream(tasks)
                        .map(Objects::requireNonNull)
                        .forEach(task -> scope.fork(Executors.callable(task)));
            }

            scope.join().throwIfFailed();

            if (eventual != null) {
                eventual.run();
            }
        } catch (ExecutionException e) {
            rethrowTaskFailure(e.getCause());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new UtilsException("Structured task execution was interrupted", e);
        }
    }

    private static void rethrowTaskFailure(Throwable cause) {
        if (cause instanceof RuntimeException runtimeException) {
            throw runtimeException;
        }
        if (cause instanceof Error error) {
            throw error;
        }
        throw new UtilsException("Structured task execution failed", cause);
    }
}
