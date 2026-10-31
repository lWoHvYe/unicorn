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

package com.unicorn.ai.support.retrieval;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.Optional;
import java.util.Set;

/**
 * Short-lived local cache for hybrid retrieval. The key includes tenant, KB, roles and topK,
 * so a cached result cannot cross an ACL boundary; writes invalidate the tenant/KB.
 */
@Component
public class RetrievalCache {

    private final Cache<Key, RetrievalResult> cache = Caffeine.newBuilder()
            .maximumSize(10_000)
            .expireAfterWrite(Duration.ofSeconds(30))
            .build();

    public Optional<RetrievalResult> get(String tenantId, String kbId, String query, int topK, Set<String> roles) {
        return Optional.ofNullable(cache.getIfPresent(new Key(tenantId, kbId, query, topK,
                roles == null ? Set.of() : Set.copyOf(roles))));
    }

    public void put(String tenantId, String kbId, String query, int topK, Set<String> roles,
                    RetrievalResult result) {
        cache.put(new Key(tenantId, kbId, query, topK, roles == null ? Set.of() : Set.copyOf(roles)), result);
    }

    public void invalidate(String tenantId, String kbId) {
        cache.asMap().keySet().removeIf(key -> key.tenantId().equals(tenantId)
                && (kbId == null || key.kbId().equals(kbId)));
    }

    private record Key(String tenantId, String kbId, String query, int topK, Set<String> roles) {
    }
}
