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

package com.unicorn.ai.support.tools;

import com.unicorn.ai.tools.domain.InventoryRepository;
import com.unicorn.ai.tools.model.InventoryItem;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class JdbcInventoryRepository implements InventoryRepository {

    private final JdbcTemplate jdbc;

    public JdbcInventoryRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public Optional<InventoryItem> find(String tenantId, String sku) {
        return jdbc.query("""
                        SELECT tenant_id, sku, name, available, reserved
                        FROM ai_inventory WHERE tenant_id = ? AND sku = ?
                        """,
                (rs, rowNum) -> new InventoryItem(rs.getString("tenant_id"), rs.getString("sku"),
                        rs.getString("name"), rs.getInt("available"), rs.getInt("reserved")),
                tenantId, sku).stream().findFirst();
    }

    @Override
    public InventoryItem save(InventoryItem item) {
        var updated = jdbc.update("""
                UPDATE ai_inventory SET name = ?, available = ?, reserved = ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND sku = ?
                """, item.name(), item.available(), item.reserved(), item.tenantId(), item.sku());
        if (updated == 0) {
            jdbc.update("""
                    INSERT INTO ai_inventory (tenant_id, sku, name, available, reserved)
                    VALUES (?, ?, ?, ?, ?)
                    """, item.tenantId(), item.sku(), item.name(), item.available(), item.reserved());
        }
        return item;
    }

    @Override
    public boolean reserve(String tenantId, String sku, int quantity) {
        return jdbc.update("""
                UPDATE ai_inventory
                SET available = available - ?, reserved = reserved + ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND sku = ? AND available >= ?
                """, quantity, quantity, tenantId, sku, quantity) == 1;
    }

    @Override
    public List<InventoryItem> findByTenant(String tenantId) {
        return jdbc.query("""
                        SELECT tenant_id, sku, name, available, reserved
                        FROM ai_inventory WHERE tenant_id = ? ORDER BY sku
                        """,
                (rs, rowNum) -> new InventoryItem(rs.getString("tenant_id"), rs.getString("sku"),
                        rs.getString("name"), rs.getInt("available"), rs.getInt("reserved")),
                tenantId);
    }
}
