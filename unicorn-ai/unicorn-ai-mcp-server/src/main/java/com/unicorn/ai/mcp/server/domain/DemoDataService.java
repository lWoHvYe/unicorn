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

package com.unicorn.ai.mcp.server.domain;

import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class DemoDataService {

    private final Map<String, Order> orders = new ConcurrentHashMap<>(Map.of(
            "A1001", new Order("A1001", "张三", "DELIVERED", new BigDecimal("299.00"), "2026-10-10", 8),
            "A1002", new Order("A1002", "李四", "SHIPPED", new BigDecimal("1299.00"), "2026-10-16", 2),
            "A1003", new Order("A1003", "王五", "CREATED", new BigDecimal("59.90"), "2026-10-18", 0),
            "A1004", new Order("A1004", "赵六", "DELIVERED", new BigDecimal("899.00"), "2026-10-17", 1)));

    private final Map<String, InventoryItem> inventory = new ConcurrentHashMap<>(Map.of(
            "SKU-1", new InventoryItem("SKU-1", "机械键盘", 12, 2),
            "SKU-2", new InventoryItem("SKU-2", "人体工学椅", 3, 0),
            "SKU-3", new InventoryItem("SKU-3", "USB-C 扩展坞", 30, 5)));

    public Order order(String orderId) {
        var order = orders.get(orderId);
        return order == null
                ? new Order(orderId, "unknown", "NOT_FOUND", BigDecimal.ZERO, "", -1)
                : order;
    }

    public RefundDecision refundEligibility(String orderId) {
        var order = order(orderId);
        if ("NOT_FOUND".equals(order.status())) {
            return new RefundDecision(orderId, false, "订单不存在");
        }
        if (!"DELIVERED".equals(order.status())) {
            return new RefundDecision(orderId, false, "订单尚未签收，不能按签收后退款规则处理");
        }
        if (order.daysSincePurchase() > 7) {
            return new RefundDecision(orderId, false, "已超过 7 天无理由退款窗口");
        }
        return new RefundDecision(orderId, true, "签收未超过 7 天，且订单状态正常");
    }

    public RefundDecision refund(String orderId) {
        var decision = refundEligibility(orderId);
        if (!decision.eligible()) {
            return decision;
        }
        var order = order(orderId);
        orders.put(orderId, new Order(order.orderId(), order.customer(), "REFUNDED",
                order.amount(), order.createdAt(), order.daysSincePurchase()));
        return new RefundDecision(orderId, true, "退款已受理，状态已更新为 REFUNDED");
    }

    public InventoryItem inventory(String sku) {
        var item = inventory.get(sku);
        return item == null ? new InventoryItem(sku, "unknown", 0, 0) : item;
    }

    public synchronized Reservation reserve(String sku, int quantity) {
        if (quantity <= 0) {
            return new Reservation(sku, quantity, null, false, "quantity 必须大于 0");
        }
        var item = inventory.get(sku);
        if (item == null) {
            return new Reservation(sku, quantity, null, false, "SKU 不存在");
        }
        if (item.available() < quantity) {
            return new Reservation(sku, quantity, null, false, "库存不足");
        }
        inventory.put(sku, new InventoryItem(item.sku(), item.name(),
                item.available() - quantity, item.reserved() + quantity));
        return new Reservation(sku, quantity, UUID.randomUUID().toString(), true, "预留成功");
    }

    public List<Order> orders() {
        return List.copyOf(orders.values());
    }

    public record Order(String orderId, String customer, String status, BigDecimal amount,
                        String createdAt, int daysSincePurchase) {
    }

    public record InventoryItem(String sku, String name, int available, int reserved) {
    }

    public record RefundDecision(String orderId, boolean eligible, String reason) {
    }

    public record Reservation(String sku, int quantity, String reservationId, boolean success, String message) {
    }
}
