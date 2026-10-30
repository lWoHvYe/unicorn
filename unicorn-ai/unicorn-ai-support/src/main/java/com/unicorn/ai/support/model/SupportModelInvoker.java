/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.unicorn.ai.support.model;

import com.unicorn.ai.support.config.SupportProperties;
import com.unicorn.ai.support.retrieval.NoContextException;
import jakarta.annotation.PreDestroy;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;

/**
 * Bounded model invocation. The HTTP client has its own timeout; this guard also
 * prevents a stuck provider call from holding an MVC/virtual thread forever.
 */
@Component
public class SupportModelInvoker {

    private final SupportProperties properties;
    private final java.util.concurrent.ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

    public SupportModelInvoker(SupportProperties properties) {
        this.properties = properties;
    }

    public <T> T call(String tenantId, Supplier<T> action) {
        var model = properties.model() == null ? new SupportProperties.Model(30000, 2) : properties.model();
        RuntimeException last = null;
        for (int attempt = 1; attempt <= Math.max(1, model.maxAttempts()); attempt++) {
            var future = CompletableFuture.supplyAsync(action, executor);
            try {
                return future.get(model.timeoutMs(), TimeUnit.MILLISECONDS);
            } catch (TimeoutException exception) {
                future.cancel(true);
                last = new ModelCallException("MODEL_TIMEOUT", "模型调用超时", exception);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                throw new ModelCallException("MODEL_INTERRUPTED", "模型调用被中断", exception);
            } catch (ExecutionException exception) {
                if (exception.getCause() instanceof NoContextException noContext) {
                    throw noContext;
                }
                last = new ModelCallException("MODEL_FAILED", "模型调用失败", exception.getCause());
            }
        }
        throw last;
    }

    public Duration timeout() {
        return Duration.ofMillis(properties.model().timeoutMs());
    }

    @PreDestroy
    void shutdown() {
        executor.shutdownNow();
    }
}
