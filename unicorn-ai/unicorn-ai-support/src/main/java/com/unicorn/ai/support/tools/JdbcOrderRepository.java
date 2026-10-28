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

import com.unicorn.ai.tools.domain.OrderRepository;
import com.unicorn.ai.tools.model.Order;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class JdbcOrderRepository implements OrderRepository {

    private final JdbcTemplate jdbc;

    public JdbcOrderRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public Optional<Order> find(String tenantId, String orderId) {
        return jdbc.query("""
                        SELECT tenant_id, order_id, customer, status, amount, created_at, days_since_purchase
                        FROM ai_order WHERE tenant_id = ? AND order_id = ?
                        """,
                (rs, rowNum) -> new Order(rs.getString("tenant_id"), rs.getString("order_id"),
                        rs.getString("customer"), rs.getString("status"), rs.getBigDecimal("amount"),
                        rs.getString("created_at"), rs.getInt("days_since_purchase")),
                tenantId, orderId).stream().findFirst();
    }

    @Override
    public Order save(Order order) {
        var updated = jdbc.update("""
                UPDATE ai_order SET customer = ?, status = ?, amount = ?, created_at = ?,
                    days_since_purchase = ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND order_id = ?
                """, order.customer(), order.status(), order.amount(), order.createdAt(),
                order.daysSincePurchase(), order.tenantId(), order.orderId());
        if (updated == 0) {
            jdbc.update("""
                    INSERT INTO ai_order (tenant_id, order_id, customer, status, amount, created_at, days_since_purchase)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                    """, order.tenantId(), order.orderId(), order.customer(), order.status(), order.amount(),
                    order.createdAt(), order.daysSincePurchase());
        }
        return order;
    }

    @Override
    public List<Order> findByTenant(String tenantId) {
        return jdbc.query("""
                        SELECT tenant_id, order_id, customer, status, amount, created_at, days_since_purchase
                        FROM ai_order WHERE tenant_id = ? ORDER BY order_id
                        """,
                (rs, rowNum) -> new Order(rs.getString("tenant_id"), rs.getString("order_id"),
                        rs.getString("customer"), rs.getString("status"), rs.getBigDecimal("amount"),
                        rs.getString("created_at"), rs.getInt("days_since_purchase")),
                tenantId);
    }
}
