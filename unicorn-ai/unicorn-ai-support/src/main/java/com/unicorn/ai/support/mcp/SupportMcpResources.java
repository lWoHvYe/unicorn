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

import com.unicorn.ai.support.security.TenantContext;
import com.unicorn.ai.tools.domain.SupportToolService;
import org.springframework.ai.mcp.annotation.McpResource;
import org.springframework.stereotype.Component;
import tools.jackson.databind.json.JsonMapper;

@Component
public class SupportMcpResources {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final SupportToolService tools;

    public SupportMcpResources(SupportToolService tools) {
        this.tools = tools;
    }

    @McpResource(uri = "unicorn://orders/{orderId}", name = "order-detail",
            description = "当前租户的订单详情", mimeType = "application/json")
    public String order(String orderId) {
        var tenant = TenantContext.current().tenantId();
        return tools.getOrder(tenant, orderId)
                .map(order -> JSON.writeValueAsString(order))
                .orElseGet(() -> JSON.writeValueAsString(java.util.Map.of("orderId", orderId, "status", "NOT_FOUND")));
    }

    @McpResource(uri = "unicorn://inventory/{sku}", name = "inventory-detail",
            description = "当前租户的 SKU 库存", mimeType = "application/json")
    public String inventory(String sku) {
        var tenant = TenantContext.current().tenantId();
        return tools.checkInventory(tenant, sku)
                .map(item -> JSON.writeValueAsString(item))
                .orElseGet(() -> JSON.writeValueAsString(java.util.Map.of("sku", sku, "available", 0)));
    }
}
