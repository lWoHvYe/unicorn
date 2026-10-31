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

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.ArrayDeque;

@Component
@ConditionalOnProperty(name = "ai.support.rate-limit.store", havingValue = "local", matchIfMissing = true)
public class LocalRateLimitStore implements RateLimitStore {

    private final Cache<String, Window> windows = Caffeine.newBuilder()
            .maximumSize(100_000)
            .expireAfterWrite(Duration.ofHours(2))
            .build();

    @Override
    public boolean tryAcquire(String key, int limit, Duration window) {
        var current = windows.get(key, ignored -> new Window());
        synchronized (current) {
            var cutoff = System.nanoTime() - window.toNanos();
            while (!current.timestamps.isEmpty() && current.timestamps.peekFirst() <= cutoff) {
                current.timestamps.removeFirst();
            }
            if (current.timestamps.size() >= limit) {
                return false;
            }
            current.timestamps.addLast(System.nanoTime());
            return true;
        }
    }

    private static final class Window {
        private final ArrayDeque<Long> timestamps = new ArrayDeque<>();
    }
}
