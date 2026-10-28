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

package com.unicorn.ai.tools.policy;

import com.unicorn.ai.tools.domain.SupportToolService;
import com.unicorn.ai.tools.model.ToolPolicy;
import com.unicorn.ai.tools.model.ToolResult;

import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;

/**
 * Registers the support tool catalog once; Chat and MCP facades both use this service.
 */
public final class SupportToolCatalog {

    private static final Set<String> AGENT_ROLES = Set.of("SUPPORT_AGENT", "SUPPORT_MANAGER", "TENANT_ADMIN");

    private SupportToolCatalog() {
    }

    public static void register(ToolInvocationService invocations, SupportToolService tools,
                                BigDecimal refundThreshold) {
        invocations.register(new ToolPolicy("get_order", false, Set.of(), false, null),
                args -> tools.getOrder(text(args, "tenantId"), text(args, "orderId"))
                        .map(ToolResult::completed)
                        .orElseGet(() -> ToolResult.completed(Map.of("orderId", text(args, "orderId"), "status", "NOT_FOUND"))));

        invocations.register(new ToolPolicy("refund_eligibility", false, Set.of(), false, null),
                args -> ToolResult.completed(tools.refundEligibility(text(args, "tenantId"), text(args, "orderId"))));

        invocations.register(new ToolPolicy("search_products", false, Set.of(), false, null),
                args -> ToolResult.completed(tools.searchProducts(text(args, "tenantId"), text(args, "keyword"))));

        invocations.register(new ToolPolicy("check_inventory", false, Set.of(), false, null),
                args -> tools.checkInventory(text(args, "tenantId"), text(args, "sku"))
                        .map(ToolResult::completed)
                        .orElseGet(() -> ToolResult.completed(Map.of("sku", text(args, "sku"), "available", 0))));

        invocations.register(new ToolPolicy("create_handoff", true, AGENT_ROLES, false, null),
                args -> ToolResult.completed(tools.handoff(text(args, "tenantId"), text(args, "reason"))));

        invocations.register(new ToolPolicy("refund_order", true, AGENT_ROLES, false, refundThreshold),
                args -> ToolResult.completed(tools.refund(text(args, "tenantId"), text(args, "orderId"))));

        invocations.register(new ToolPolicy("reserve_inventory", true, AGENT_ROLES, false, null),
                args -> ToolResult.completed(tools.reserveInventory(text(args, "tenantId"),
                        text(args, "sku"), number(args, "quantity").intValue())));
    }

    private static String text(Map<String, Object> args, String key) {
        return java.util.Objects.toString(args.get(key), "");
    }

    private static Number number(Map<String, Object> args, String key) {
        var value = args.get(key);
        return value instanceof Number number ? number : 0;
    }
}
