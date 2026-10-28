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

package com.unicorn.ai.tools.domain;

import com.unicorn.ai.tools.model.Order;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class InMemoryOrderRepository implements OrderRepository {

    private final Map<String, Order> orders = new ConcurrentHashMap<>();

    public InMemoryOrderRepository() {
        seed("demo");
    }

    public void seed(String tenantId) {
        orders.put(key(tenantId, "A1001"),
                new Order(tenantId, "A1001", "张三", "DELIVERED", new BigDecimal("299.00"), "2026-10-10", 8));
        orders.put(key(tenantId, "A1002"),
                new Order(tenantId, "A1002", "李四", "SHIPPED", new BigDecimal("1299.00"), "2026-10-16", 2));
        orders.put(key(tenantId, "A1003"),
                new Order(tenantId, "A1003", "王五", "CREATED", new BigDecimal("59.90"), "2026-10-18", 0));
        orders.put(key(tenantId, "A1004"),
                new Order(tenantId, "A1004", "赵六", "DELIVERED", new BigDecimal("899.00"), "2026-10-17", 1));
    }

    @Override
    public Optional<Order> find(String tenantId, String orderId) {
        return Optional.ofNullable(orders.get(key(tenantId, orderId)));
    }

    @Override
    public Order save(Order order) {
        orders.put(key(order.tenantId(), order.orderId()), order);
        return order;
    }

    @Override
    public List<Order> findByTenant(String tenantId) {
        return orders.entrySet().stream()
                .filter(entry -> entry.getKey().startsWith(tenantId + ":"))
                .map(Map.Entry::getValue)
                .toList();
    }

    private String key(String tenantId, String orderId) {
        return tenantId + ":" + orderId;
    }
}
