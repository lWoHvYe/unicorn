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

package com.unicorn.ai.support.observability;

import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.stereotype.Component;

import java.time.Duration;

@Component
public class SupportMetrics {

    private final MeterRegistry registry;

    public SupportMetrics(MeterRegistry registry) {
        this.registry = registry;
    }

    public void chat(String tenantId, String status, long latencyNanos) {
        registry.timer("ai.support.chat", "tenant", tenantId, "status", status)
                .record(Duration.ofNanos(latencyNanos));
    }

    public void ingest(String tenantId, String status) {
        registry.counter("ai.support.ingest.jobs", "tenant", tenantId, "status", status).increment();
    }

    public void tool(String tenantId, String tool, String status) {
        registry.counter("ai.support.tool.calls", "tenant", tenantId, "tool", tool, "status", status).increment();
    }
}
