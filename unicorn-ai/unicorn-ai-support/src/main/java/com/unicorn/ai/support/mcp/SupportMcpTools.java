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

package com.unicorn.ai.support.mcp;

import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.security.TenantContext;
import com.unicorn.ai.support.observability.SupportMetrics;
import com.unicorn.ai.support.ratelimit.RateLimiterService;
import com.unicorn.ai.tools.domain.SupportToolService;
import com.unicorn.ai.tools.model.ToolExecutionContext;
import com.unicorn.ai.tools.model.ToolResult;
import com.unicorn.ai.tools.policy.ToolInvocationService;
import org.slf4j.MDC;
import org.springframework.ai.mcp.annotation.McpTool;
import org.springframework.ai.mcp.annotation.McpToolParam;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

/**
 * MCP surface for external agents. It shares the same policy/audit service as the Chat tools.
 */
@Component
public class SupportMcpTools {

    private final ToolInvocationService invocations;
    private final SupportToolService tools;
    private final SupportMetrics metrics;
    private final RateLimiterService rateLimiter;

    public SupportMcpTools(ToolInvocationService invocations, SupportToolService tools, SupportMetrics metrics,
                           RateLimiterService rateLimiter) {
        this.invocations = invocations;
        this.tools = tools;
        this.metrics = metrics;
        this.rateLimiter = rateLimiter;
    }

    @McpTool(name = "get_order", description = "按订单号查询订单详情",
            annotations = @McpTool.McpAnnotations(readOnlyHint = true, destructiveHint = false, openWorldHint = false))
    public ToolResult getOrder(@McpToolParam(description = "订单号", required = true) String orderId) {
        var context = context("");
        return execute(context, "get_order", Map.of("tenantId", context.tenantId(), "orderId", orderId));
    }

    @McpTool(name = "refund_eligibility", description = "判断订单是否满足退款条件",
            annotations = @McpTool.McpAnnotations(readOnlyHint = true, destructiveHint = false, openWorldHint = false))
    public ToolResult refundEligibility(@McpToolParam(description = "订单号", required = true) String orderId) {
        var context = context("");
        return execute(context, "refund_eligibility",
                Map.of("tenantId", context.tenantId(), "orderId", orderId));
    }

    @McpTool(name = "refund_order", description = "对满足条件的订单发起退款（写操作，可能需要审批）",
            annotations = @McpTool.McpAnnotations(destructiveHint = true, idempotentHint = true))
    public ToolResult refundOrder(@McpToolParam(description = "订单号", required = true) String orderId) {
        var context = context("");
        var amount = tools.getOrder(context.tenantId(), orderId)
                .map(order -> order.amount()).orElse(BigDecimal.ZERO);
        return execute(context, "refund_order",
                Map.of("tenantId", context.tenantId(), "orderId", orderId, "amount", amount));
    }

    @McpTool(name = "check_inventory", description = "查询 SKU 可用库存",
            annotations = @McpTool.McpAnnotations(readOnlyHint = true, destructiveHint = false))
    public ToolResult checkInventory(@McpToolParam(description = "SKU", required = true) String sku) {
        var context = context("");
        return execute(context, "check_inventory",
                Map.of("tenantId", context.tenantId(), "sku", sku));
    }

    @McpTool(name = "reserve_inventory", description = "预留指定数量库存（写操作）",
            annotations = @McpTool.McpAnnotations(destructiveHint = true, idempotentHint = false))
    public ToolResult reserveInventory(@McpToolParam(description = "SKU", required = true) String sku,
                                       @McpToolParam(description = "数量", required = true) int quantity) {
        var context = context("");
        return execute(context, "reserve_inventory",
                Map.of("tenantId", context.tenantId(), "sku", sku, "quantity", quantity));
    }

    @McpTool(name = "create_handoff", description = "创建人工客服工单",
            annotations = @McpTool.McpAnnotations(destructiveHint = true))
    public ToolResult createHandoff(@McpToolParam(description = "原因", required = true) String reason) {
        var context = context("");
        return execute(context, "create_handoff",
                Map.of("tenantId", context.tenantId(), "reason", reason));
    }

    private ToolResult execute(ToolExecutionContext context, String tool, Map<String, Object> args) {
        rateLimiter.checkMcp(context.tenantId());
        var result = invocations.execute(context, tool, args);
        metrics.tool(context.tenantId(), tool, result.status().name());
        return result;
    }

    private ToolExecutionContext context(String idempotencyKey) {
        AiPrincipal principal = TenantContext.current();
        var trace = MDC.get("traceId");
        return new ToolExecutionContext(principal.tenantId(), principal.userId(), principal.roles(),
                null, idempotencyKey, trace == null ? UUID.randomUUID().toString() : trace);
    }
}
