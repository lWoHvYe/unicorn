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

package com.unicorn.ai.chat.tools;

import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * 供模型调用的本地工具。真实项目里应替换为 Service/Repository，并做好权限与幂等。
 */
@Component
public class ShopTools {

    private final Map<String, Order> orders = Map.of(
            "A1001", new Order("A1001", "张三", "DELIVERED", new BigDecimal("299.00"), "2026-10-10", 8),
            "A1002", new Order("A1002", "李四", "SHIPPED", new BigDecimal("1299.00"), "2026-10-16", 2),
            "A1003", new Order("A1003", "王五", "CREATED", new BigDecimal("59.90"), "2026-10-18", 0),
            "A1004", new Order("A1004", "赵六", "DELIVERED", new BigDecimal("899.00"), "2026-10-17", 1));

    private final List<Product> products = List.of(
            new Product("SKU-1", "机械键盘", new BigDecimal("499.00"), 12),
            new Product("SKU-2", "人体工学椅", new BigDecimal("1999.00"), 3),
            new Product("SKU-3", "USB-C 扩展坞", new BigDecimal("299.00"), 30));

    @Tool(description = "按订单号查询订单详情。订单号示例：A1001")
    public Order getOrder(
            @ToolParam(description = "订单号，例如 A1001") String orderId,
            ToolContext context) {
        var tenant = tenant(context);
        var order = orders.get(orderId);
        if (order == null) {
            return new Order(orderId, "unknown", "NOT_FOUND", BigDecimal.ZERO, "", -1);
        }
        return order.withTenant(tenant);
    }

    @Tool(description = "判断订单是否满足退款条件，并返回判断原因")
    public RefundDecision refundEligibility(
            @ToolParam(description = "订单号，例如 A1001") String orderId,
            ToolContext context) {
        var order = orders.get(orderId);
        if (order == null) {
            return new RefundDecision(orderId, false, "订单不存在");
        }
        if (!"DELIVERED".equals(order.status())) {
            return new RefundDecision(orderId, false, "订单尚未签收，不能按签收后退款规则处理");
        }
        if (order.daysSincePurchase() > 7) {
            return new RefundDecision(orderId, false, "已超过 7 天无理由退款窗口");
        }
        return new RefundDecision(orderId, true, "签收未超过 7 天，且订单状态正常（tenant=" + tenant(context) + "）");
    }

    @Tool(description = "按关键词搜索商品，返回 SKU、名称、价格和库存")
    public List<Product> searchProducts(
            @ToolParam(description = "商品关键词，例如 键盘") String keyword) {
        var lowered = Objects.toString(keyword, "").toLowerCase(Locale.ROOT);
        return products.stream()
                .filter(product -> product.name().toLowerCase(Locale.ROOT).contains(lowered)
                        || product.sku().toLowerCase(Locale.ROOT).contains(lowered))
                .toList();
    }

    @Tool(description = "获取当前时间的 ISO-8601 字符串")
    public String now() {
        return java.time.OffsetDateTime.now().toString();
    }

    @Tool(description = "转人工客服。该工具的结果会直接返回给调用方，不再交给模型二次总结",
            returnDirect = true)
    public String handoffToHuman(
            @ToolParam(description = "转人工原因") String reason) {
        return "已创建人工工单，原因：" + reason;
    }

    private String tenant(ToolContext context) {
        return Objects.toString(context.getContext().getOrDefault("tenant", "demo"));
    }

    public record Order(String orderId, String customer, String status, BigDecimal amount,
                        String createdAt, int daysSincePurchase, String tenant) {

        public Order(String orderId, String customer, String status, BigDecimal amount,
                     String createdAt, int daysSincePurchase) {
            this(orderId, customer, status, amount, createdAt, daysSincePurchase, null);
        }

        public Order withTenant(String tenant) {
            return new Order(orderId, customer, status, amount, createdAt, daysSincePurchase, tenant);
        }
    }

    public record RefundDecision(String orderId, boolean eligible, String reason) {
    }

    public record Product(String sku, String name, BigDecimal price, int stock) {
    }
}
