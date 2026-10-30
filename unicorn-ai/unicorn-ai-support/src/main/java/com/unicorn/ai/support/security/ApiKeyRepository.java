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

package com.unicorn.ai.support.security;

import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
public class ApiKeyRepository {

    private final JdbcTemplate jdbcTemplate;
    private final CacheManager cacheManager;

    public ApiKeyRepository(JdbcTemplate jdbcTemplate, CacheManager cacheManager) {
        this.jdbcTemplate = jdbcTemplate;
        this.cacheManager = cacheManager;
    }

    @Cacheable(cacheNames = "ai-api-key", key = "#keyHash")
    public Optional<ApiKeyPrincipal> findActiveByHash(String keyHash) {
        return jdbcTemplate.query("""
                        SELECT tenant_id, user_id, roles
                        FROM ai_api_key
                        WHERE key_hash = ? AND enabled = TRUE
                          AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)
                        """,
                (rs, rowNum) -> new ApiKeyPrincipal(
                        rs.getString("tenant_id"),
                        rs.getString("user_id"),
                        Set.of(rs.getString("roles").split(","))),
                keyHash).stream().findFirst();
    }

    public CreatedKey create(String tenantId, String userId, String roles) {
        var rawKey = java.util.UUID.randomUUID().toString().replace("-", "");
        var id = java.util.UUID.randomUUID().toString();
        jdbcTemplate.update("""
                INSERT INTO ai_api_key (id, tenant_id, user_id, key_hash, roles, enabled)
                VALUES (?, ?, ?, ?, ?, TRUE)
                """, id, tenantId, userId, com.unicorn.ai.support.util.Hashing.sha256(rawKey), roles);
        return new CreatedKey(id, rawKey);
    }

    public void revoke(String tenantId, String keyId) {
        var hash = jdbcTemplate.queryForList("""
                SELECT key_hash FROM ai_api_key WHERE tenant_id = ? AND id = ?
                """, String.class, tenantId, keyId).stream().findFirst()
                .orElseThrow(() -> new java.util.NoSuchElementException("API Key 不存在"));
        jdbcTemplate.update("UPDATE ai_api_key SET enabled = FALSE WHERE tenant_id = ? AND id = ?",
                tenantId, keyId);
        var cache = cacheManager.getCache("ai-api-key");
        if (cache != null && hash != null) {
            cache.evict(hash);
        }
    }

    public record CreatedKey(String id, String apiKey) {
    }

    public record ApiKeyPrincipal(String tenantId, String userId, Set<String> roles)
            implements java.io.Serializable {
    }
}
