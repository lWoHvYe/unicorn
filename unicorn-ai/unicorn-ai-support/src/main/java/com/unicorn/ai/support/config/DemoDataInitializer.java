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
                INSERT INTO ai_api_key (id, tenant_id, user_id, key_hash, roles, enabled)
                VALUES (?, ?, ?, ?, ?, TRUE)
                """, "agent-key", "demo", "agent-user", Hashing.sha256("agent-key"), "SUPPORT_AGENT");
        jdbcTemplate.update("""
                INSERT INTO ai_knowledge_base (id, tenant_id, name, description, status)
                VALUES (?, ?, ?, ?, 'ACTIVE')
                """, "demo-kb", "demo", "Demo Knowledge Base", "Seed knowledge base");

        seedDomain("demo");

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

    private void seedDomain(String tenantId) {
        jdbcTemplate.update("""
                INSERT INTO ai_order (tenant_id, order_id, customer, status, amount, created_at, days_since_purchase)
                VALUES (?, 'A1001', '张三', 'DELIVERED', 299.00, '2026-10-10', 8)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_order (tenant_id, order_id, customer, status, amount, created_at, days_since_purchase)
                VALUES (?, 'A1002', '李四', 'SHIPPED', 1299.00, '2026-10-16', 2)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_order (tenant_id, order_id, customer, status, amount, created_at, days_since_purchase)
                VALUES (?, 'A1003', '王五', 'CREATED', 59.90, '2026-10-18', 0)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_order (tenant_id, order_id, customer, status, amount, created_at, days_since_purchase)
                VALUES (?, 'A1004', '赵六', 'DELIVERED', 899.00, '2026-10-17', 1)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_inventory (tenant_id, sku, name, available, reserved)
                VALUES (?, 'SKU-1', '机械键盘', 12, 2)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_inventory (tenant_id, sku, name, available, reserved)
                VALUES (?, 'SKU-2', '人体工学椅', 3, 0)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_inventory (tenant_id, sku, name, available, reserved)
                VALUES (?, 'SKU-3', 'USB-C 扩展坞', 30, 5)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_product (tenant_id, sku, name, price, stock)
                VALUES (?, 'SKU-1', '机械键盘', 499.00, 12)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_product (tenant_id, sku, name, price, stock)
                VALUES (?, 'SKU-2', '人体工学椅', 1999.00, 3)
                """, tenantId);
        jdbcTemplate.update("""
                INSERT INTO ai_product (tenant_id, sku, name, price, stock)
                VALUES (?, 'SKU-3', 'USB-C 扩展坞', 299.00, 30)
                """, tenantId);
    }
}
