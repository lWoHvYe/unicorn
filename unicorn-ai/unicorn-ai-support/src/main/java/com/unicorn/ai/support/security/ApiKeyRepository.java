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

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
public class ApiKeyRepository {

    private final JdbcTemplate jdbcTemplate;

    public ApiKeyRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

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

    public record ApiKeyPrincipal(String tenantId, String userId, Set<String> roles) {
    }
}
