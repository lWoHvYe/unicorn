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
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.security.TenantContext;
import io.github.resilience4j.bulkhead.Bulkhead;
import io.github.resilience4j.bulkhead.BulkheadConfig;
import io.github.resilience4j.bulkhead.BulkheadFullException;
import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.retry.Retry;
import io.github.resilience4j.retry.RetryConfig;
import io.micrometer.observation.Observation;
import io.micrometer.observation.ObservationRegistry;
import jakarta.annotation.PreDestroy;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.web.client.RestClientResponseException;
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
    private final ObservationRegistry observations;
    private final java.util.concurrent.ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();
    private final CircuitBreaker circuitBreaker;
    private final Retry retry;
    private final Bulkhead bulkhead;

    public SupportModelInvoker(SupportProperties properties, ObjectProvider<ObservationRegistry> observations) {
        this.properties = properties;
        this.observations = observations.getIfAvailable(() -> ObservationRegistry.NOOP);
        var model = properties.model() == null ? new SupportProperties.Model(30000, 2, "") : properties.model();
        this.circuitBreaker = CircuitBreaker.of("support-model", CircuitBreakerConfig.custom()
                .failureRateThreshold(50)
                .slidingWindowSize(20)
                .minimumNumberOfCalls(10)
                .waitDurationInOpenState(Duration.ofSeconds(30))
                .build());
        this.retry = Retry.of("support-model", RetryConfig.custom()
                .maxAttempts(Math.max(1, model.maxAttempts()))
                .waitDuration(Duration.ofMillis(200))
                .retryOnException(throwable -> throwable instanceof ModelCallException failure
                        && failure.retryable())
                .ignoreExceptions(NoContextException.class)
                .build());
        this.bulkhead = Bulkhead.of("support-model", BulkheadConfig.custom()
                .maxConcurrentCalls(16)
                .maxWaitDuration(Duration.ZERO)
                .build());
    }

    public <T> T call(String tenantId, Supplier<T> action) {
        return Observation.createNotStarted("ai.support.model", observations)
                .lowCardinalityKeyValue("tenant", tenantId)
                .observe(() -> invoke(tenantId, action));
    }

    public <T> T call(String tenantId, Supplier<T> primary, Supplier<T> fallback) {
        try {
            return call(tenantId, primary);
        } catch (ModelCallException | CallNotPermittedException | BulkheadFullException exception) {
            if (fallback == null) {
                throw exception;
            }
            return Observation.createNotStarted("ai.support.model", observations)
                    .lowCardinalityKeyValue("tenant", tenantId)
                    .lowCardinalityKeyValue("route", "fallback")
                    .observe(() -> invokeFallback(tenantId, fallback));
        }
    }

    private <T> T invoke(String tenantId, Supplier<T> action) {
        return Bulkhead.decorateSupplier(bulkhead,
                Retry.decorateSupplier(retry,
                        CircuitBreaker.decorateSupplier(circuitBreaker,
                                () -> executeOnce(tenantId, action)))).get();
    }

    private <T> T invokeFallback(String tenantId, Supplier<T> action) {
        return Bulkhead.decorateSupplier(bulkhead,
                Retry.decorateSupplier(retry, () -> executeOnce(tenantId, action))).get();
    }

    private <T> T executeOnce(String tenantId, Supplier<T> action) {
        var model = properties.model() == null ? new SupportProperties.Model(30000, 2, "") : properties.model();
        var future = CompletableFuture.supplyAsync(() -> TenantContext.callUnchecked(
                new AiPrincipal(tenantId, "model", java.util.Set.of()), action::get), executor);
        try {
            return future.get(model.timeoutMs(), TimeUnit.MILLISECONDS);
        } catch (TimeoutException exception) {
            future.cancel(true);
            throw new ModelCallException("MODEL_TIMEOUT", "模型调用超时", exception);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new ModelCallException("MODEL_INTERRUPTED", "模型调用被中断", exception, false);
        } catch (ExecutionException exception) {
            if (exception.getCause() instanceof NoContextException noContext) {
                throw noContext;
            }
            throw new ModelCallException("MODEL_FAILED", "模型调用失败", exception.getCause(),
                    retryable(exception.getCause()));
        }
    }

    private boolean retryable(Throwable cause) {
        if (cause instanceof RestClientResponseException response) {
            var status = response.getStatusCode().value();
            return status == 429 || status >= 500;
        }
        var name = cause == null ? "" : cause.getClass().getName();
        return !name.contains("NonTransient") && !name.contains("Authentication");
    }

    public Duration timeout() {
        var model = properties.model() == null ? new SupportProperties.Model(30000, 2, "") : properties.model();
        return Duration.ofMillis(model.timeoutMs());
    }

    @PreDestroy
    void shutdown() {
        executor.shutdownNow();
    }
}
