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

import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.security.TenantContext;
import com.unicorn.ai.support.observability.SupportMetrics;
import com.unicorn.ai.tools.domain.SupportToolService;
import com.unicorn.ai.tools.model.ToolExecutionContext;
import com.unicorn.ai.tools.model.ToolResult;
import com.unicorn.ai.tools.policy.ToolInvocationService;
import org.slf4j.MDC;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

@Component
public class SupportToolFacade {

    private final ToolInvocationService invocations;
    private final SupportToolService tools;
    private final SupportMetrics metrics;

    public SupportToolFacade(ToolInvocationService invocations, SupportToolService tools, SupportMetrics metrics) {
        this.invocations = invocations;
        this.tools = tools;
        this.metrics = metrics;
    }

    @Tool(description = "按订单号查询订单详情。订单号示例：A1001")
    public ToolResult getOrder(@ToolParam(description = "订单号") String orderId, ToolContext context) {
        var ctx = context(context);
        return execute(ctx, "get_order", Map.of("tenantId", ctx.tenantId(), "orderId", orderId));
    }

    @Tool(description = "判断订单是否满足退款条件，并返回判断原因")
    public ToolResult refundEligibility(@ToolParam(description = "订单号") String orderId, ToolContext context) {
        var ctx = context(context);
        return execute(ctx, "refund_eligibility",
                Map.of("tenantId", ctx.tenantId(), "orderId", orderId));
    }

    @Tool(description = "按关键词搜索商品，返回 SKU、名称、价格和库存")
    public ToolResult searchProducts(@ToolParam(description = "商品关键词") String keyword, ToolContext context) {
        var ctx = context(context);
        return execute(ctx, "search_products",
                Map.of("tenantId", ctx.tenantId(), "keyword", keyword));
    }

    @Tool(description = "按 SKU 查询可用库存与预留库存")
    public ToolResult checkInventory(@ToolParam(description = "SKU") String sku, ToolContext context) {
        var ctx = context(context);
        return execute(ctx, "check_inventory",
                Map.of("tenantId", ctx.tenantId(), "sku", sku));
    }

    @Tool(description = "转人工客服，创建人工工单")
    public ToolResult handoff(@ToolParam(description = "转人工原因") String reason, ToolContext context) {
        var ctx = context(context);
        return execute(ctx, "create_handoff",
                Map.of("tenantId", ctx.tenantId(), "reason", reason));
    }

    @Tool(description = "对满足条件的订单发起退款。写操作，可能需要人工审批")
    public ToolResult refundOrder(@ToolParam(description = "订单号") String orderId, ToolContext context) {
        var ctx = context(context);
        var amount = tools.getOrder(ctx.tenantId(), orderId)
                .map(order -> order.amount())
                .orElse(BigDecimal.ZERO);
        return execute(ctx, "refund_order",
                Map.of("tenantId", ctx.tenantId(), "orderId", orderId, "amount", amount));
    }

    @Tool(description = "按 SKU 预留指定数量库存。写操作")
    public ToolResult reserveInventory(@ToolParam(description = "SKU") String sku,
                                       @ToolParam(description = "预留数量") int quantity,
                                       ToolContext context) {
        var ctx = context(context);
        return execute(ctx, "reserve_inventory",
                Map.of("tenantId", ctx.tenantId(), "sku", sku, "quantity", quantity));
    }

    private ToolResult execute(ToolExecutionContext context, String tool, Map<String, Object> args) {
        ToolEventContext.emit("tool", Map.of("tool", tool, "status", "RUNNING"));
        var result = invocations.execute(context, tool, args);
        metrics.tool(context.tenantId(), tool, result.status().name());
        ToolEventContext.emit("tool", Map.of("tool", tool, "status", result.status().name()));
        if (result.status() == com.unicorn.ai.tools.model.ToolStatus.PENDING_APPROVAL) {
            ToolEventContext.emit("approval", Map.of("tool", tool, "approvalId", result.approvalId(),
                    "status", "PENDING"));
        }
        return result;
    }

    private ToolExecutionContext context(ToolContext context) {
        var values = context == null || context.getContext() == null ? Map.<String, Object>of() : context.getContext();
        var principal = principal();
        var tenantId = text(values, "tenant", principal.tenantId());
        var userId = text(values, "userId", principal.userId());
        var roles = roles(values.get("roles"), principal.roles());
        var sessionId = text(values, "sessionId", "");
        var idempotencyKey = text(values, "idempotencyKey", "");
        var traceId = text(values, "traceId", traceId());
        return new ToolExecutionContext(tenantId, userId, roles, sessionId, idempotencyKey, traceId);
    }

    private AiPrincipal principal() {
        try {
            return TenantContext.current();
        } catch (RuntimeException exception) {
            return new AiPrincipal("demo", "demo-user", Set.of("SUPPORT_AGENT"));
        }
    }

    private String text(Map<String, Object> values, String key, String fallback) {
        var value = values.get(key);
        return value == null || value.toString().isBlank() ? fallback : value.toString();
    }

    private Set<String> roles(Object value, Set<String> fallback) {
        if (value == null || value.toString().isBlank()) {
            return fallback;
        }
        return Set.of(value.toString().split(","));
    }

    private String traceId() {
        var trace = MDC.get("traceId");
        return trace == null || trace.isBlank() ? UUID.randomUUID().toString() : trace;
    }
}
