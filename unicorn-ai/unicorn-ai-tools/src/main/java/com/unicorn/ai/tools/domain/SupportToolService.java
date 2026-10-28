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

import com.unicorn.ai.tools.model.HandoffTicket;
import com.unicorn.ai.tools.model.InventoryItem;
import com.unicorn.ai.tools.model.Order;
import com.unicorn.ai.tools.model.Product;
import com.unicorn.ai.tools.model.RefundDecision;
import com.unicorn.ai.tools.model.Reservation;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Domain-level tools. Tenant is always an explicit argument; no global "current tenant".
 */
public class SupportToolService {

    private final OrderRepository orders;
    private final InventoryRepository inventory;
    private final ProductRepository products;

    public SupportToolService(OrderRepository orders, InventoryRepository inventory, ProductRepository products) {
        this.orders = orders;
        this.inventory = inventory;
        this.products = products;
    }

    public Optional<Order> getOrder(String tenantId, String orderId) {
        return orders.find(tenantId, orderId);
    }

    public RefundDecision refundEligibility(String tenantId, String orderId) {
        var order = orders.find(tenantId, orderId);
        if (order.isEmpty()) {
            return new RefundDecision(orderId, false, "订单不存在");
        }
        var value = order.get();
        if (!"DELIVERED".equals(value.status())) {
            return new RefundDecision(orderId, false, "订单尚未签收，不能按签收后退款规则处理");
        }
        if (value.daysSincePurchase() > 7) {
            return new RefundDecision(orderId, false, "已超过 7 天无理由退款窗口");
        }
        return new RefundDecision(orderId, true, "签收未超过 7 天，且订单状态正常");
    }

    public RefundDecision refund(String tenantId, String orderId) {
        var decision = refundEligibility(tenantId, orderId);
        if (!decision.eligible()) {
            return decision;
        }
        var order = orders.find(tenantId, orderId).orElseThrow();
        orders.save(order.refunded());
        return new RefundDecision(orderId, true, "退款已受理，订单状态更新为 REFUNDED");
    }

    public List<Product> searchProducts(String tenantId, String keyword) {
        return products.search(tenantId, keyword);
    }

    public Optional<InventoryItem> checkInventory(String tenantId, String sku) {
        return inventory.find(tenantId, sku);
    }

    public synchronized Reservation reserveInventory(String tenantId, String sku, int quantity) {
        if (quantity <= 0) {
            return new Reservation(tenantId, sku, quantity, null, false, "quantity 必须大于 0");
        }
        var item = inventory.find(tenantId, sku);
        if (item.isEmpty()) {
            return new Reservation(tenantId, sku, quantity, null, false, "SKU 不存在");
        }
        if (!item.get().canReserve(quantity)) {
            return new Reservation(tenantId, sku, quantity, null, false, "库存不足");
        }
        var updated = item.get().reserved(quantity);
        inventory.save(updated);
        return new Reservation(tenantId, sku, quantity, UUID.randomUUID().toString(), true, "预留成功");
    }

    public HandoffTicket handoff(String tenantId, String reason) {
        return new HandoffTicket(UUID.randomUUID().toString(), tenantId, reason, "OPEN");
    }
}
