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

package com.unicorn.ai.support.config;

import com.unicorn.ai.support.util.Hashing;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.core.annotation.Order;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

@Order(1)
@Component
@ConditionalOnProperty(name = "ai.support.demo.seed", havingValue = "true")
public class DemoDataInitializer implements ApplicationRunner {

    private final JdbcTemplate jdbcTemplate;

    public DemoDataInitializer(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public void run(ApplicationArguments args) {
        var tenants = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM ai_tenant", Integer.class);
        if (tenants != null && tenants > 0) {
            return;
        }
        jdbcTemplate.update("INSERT INTO ai_tenant (id, name, status) VALUES (?, ?, ?)",
                "demo", "Demo Tenant", "ACTIVE");
        jdbcTemplate.update("""
                INSERT INTO ai_api_key (id, tenant_id, user_id, key_hash, roles, enabled)
                VALUES (?, ?, ?, ?, ?, TRUE)
                """, "demo-key", "demo", "demo-user", Hashing.sha256("demo-key"),
                "TENANT_ADMIN,SUPPORT_MANAGER,SUPPORT_AGENT");
        jdbcTemplate.update("""
                INSERT INTO ai_knowledge_base (id, tenant_id, name, description, status)
                VALUES (?, ?, ?, ?, 'ACTIVE')
                """, "demo-kb", "demo", "Demo Knowledge Base", "Seed knowledge base");

        jdbcTemplate.update("INSERT INTO ai_tenant (id, name, status) VALUES (?, ?, ?)",
                "other", "Other Tenant", "ACTIVE");
        jdbcTemplate.update("""
                INSERT INTO ai_api_key (id, tenant_id, user_id, key_hash, roles, enabled)
                VALUES (?, ?, ?, ?, ?, TRUE)
                """, "other-key", "other", "other-user", Hashing.sha256("other-key"),
                "TENANT_ADMIN,SUPPORT_MANAGER,SUPPORT_AGENT");
        jdbcTemplate.update("""
                INSERT INTO ai_knowledge_base (id, tenant_id, name, description, status)
                VALUES (?, ?, ?, ?, 'ACTIVE')
                """, "other-kb", "other", "Other Knowledge Base", "Tenant isolation test");
    }
}
