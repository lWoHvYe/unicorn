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

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.List;
import java.util.UUID;

@Slf4j
@Component
@ConditionalOnProperty(name = "ai.support.rate-limit.store", havingValue = "redis")
public class RedisRateLimitStore implements RateLimitStore {

    private static final String PREFIX = "ai:support:rate:";
    private static final DefaultRedisScript<Long> SCRIPT = new DefaultRedisScript<>("""
            local now = tonumber(ARGV[1])
            local window = tonumber(ARGV[2])
            local limit = tonumber(ARGV[3])
            redis.call('ZREMRANGEBYSCORE', KEYS[1], 0, now - window)
            local current = redis.call('ZCARD', KEYS[1])
            if current >= limit then
              return 0
            end
            redis.call('ZADD', KEYS[1], now, ARGV[4])
            redis.call('PEXPIRE', KEYS[1], window)
            return 1
            """, Long.class);

    private final StringRedisTemplate redis;

    public RedisRateLimitStore(StringRedisTemplate redis) {
        this.redis = redis;
    }

    @Override
    public boolean tryAcquire(String key, int limit, Duration window) {
        try {
            var now = System.currentTimeMillis();
            var result = redis.execute(SCRIPT, List.of(PREFIX + key),
                    String.valueOf(now), String.valueOf(window.toMillis()), String.valueOf(limit),
                    UUID.randomUUID().toString());
            return result != null && result == 1L;
        } catch (RuntimeException exception) {
            // Fail closed: a Redis outage must not silently remove tenant protection.
            log.error("Redis rate limiter unavailable", exception);
            return false;
        }
    }
}
