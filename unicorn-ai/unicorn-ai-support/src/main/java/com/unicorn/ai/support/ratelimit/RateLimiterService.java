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

package com.unicorn.ai.support.ratelimit;

import com.unicorn.ai.support.config.SupportProperties;
import org.springframework.stereotype.Component;

import java.time.Duration;

@Component
public class RateLimiterService {

    private final SupportProperties properties;
    private final RateLimitStore store;

    public RateLimiterService(SupportProperties properties, RateLimitStore store) {
        this.properties = properties;
        this.store = store;
    }

    public void checkChat(String tenantId) {
        check("chat:" + tenantId, properties.rateLimit().chatPerMinute(), Duration.ofMinutes(1));
    }

    public void checkIngest(String tenantId) {
        check("ingest:" + tenantId, properties.rateLimit().ingestPerHour(), Duration.ofHours(1));
    }

    public void checkMcp(String tenantId) {
        check("mcp:" + tenantId, properties.rateLimit().mcpPerMinute(), Duration.ofMinutes(1));
    }

    private void check(String key, int limit, Duration window) {
        if (!store.tryAcquire(key, limit, window)) {
            throw new RateLimitException("请求过于频繁，请稍后再试");
        }
    }
}
