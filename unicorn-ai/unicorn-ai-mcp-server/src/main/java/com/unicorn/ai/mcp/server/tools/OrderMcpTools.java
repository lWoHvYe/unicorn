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

package com.unicorn.ai.mcp.server.tools;

import com.unicorn.ai.mcp.server.domain.DemoDataService;
import com.unicorn.ai.mcp.server.domain.DemoDataService.Order;
import com.unicorn.ai.mcp.server.domain.DemoDataService.RefundDecision;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.mcp.annotation.McpTool;
import org.springframework.ai.mcp.annotation.McpToolParam;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class OrderMcpTools {

    private final DemoDataService data;

    @McpTool(name = "get_order", description = "按订单号查询订单详情",
            annotations = @McpTool.McpAnnotations(readOnlyHint = true, destructiveHint = false,
                    openWorldHint = false))
    public Order getOrder(
            @McpToolParam(description = "订单号，例如 A1001", required = true) String orderId) {
        return data.order(orderId);
    }

    @McpTool(name = "refund_eligibility", description = "判断订单是否满足退款条件",
            annotations = @McpTool.McpAnnotations(readOnlyHint = true, destructiveHint = false,
                    openWorldHint = false))
    public RefundDecision refundEligibility(
            @McpToolParam(description = "订单号，例如 A1001", required = true) String orderId) {
        return data.refundEligibility(orderId);
    }

    @McpTool(name = "refund_order", description = "对满足条件的订单发起退款（写操作，幂等）",
            annotations = @McpTool.McpAnnotations(destructiveHint = true, idempotentHint = true,
                    openWorldHint = false))
    public RefundDecision refundOrder(
            @McpToolParam(description = "订单号，例如 A1001", required = true) String orderId) {
        return data.refund(orderId);
    }
}
