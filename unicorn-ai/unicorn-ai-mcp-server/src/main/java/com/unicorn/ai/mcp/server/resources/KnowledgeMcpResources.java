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

package com.unicorn.ai.mcp.server.resources;

import com.unicorn.ai.mcp.server.domain.DemoDataService;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.mcp.annotation.McpResource;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class KnowledgeMcpResources {

    private final DemoDataService data;

    @McpResource(uri = "unicorn://orders/{orderId}", name = "order-detail",
            description = "订单详情 JSON 资源", mimeType = "application/json")
    public String orderDetail(String orderId) {
        var order = data.order(orderId);
        return """
                {"orderId":"%s","customer":"%s","status":"%s","amount":%s,"createdAt":"%s"}
                """.formatted(order.orderId(), order.customer(), order.status(), order.amount(), order.createdAt());
    }

    @McpResource(uri = "unicorn://runbook/deploy", name = "deploy-runbook",
            description = "Unicorn AI 模块部署与回滚手册", mimeType = "text/markdown")
    public String deployRunbook() {
        return """
                # Unicorn AI 部署手册

                1. 先启动基础设施：MySQL、Redis、RabbitMQ。
                2. 配置 OPENAI_API_KEY、OPENAI_BASE_URL、OPENAI_CHAT_MODEL。
                3. 启动 unicorn-ai-mcp-server，确认 /actuator/health 为 UP。
                4. 以 MCP_CLIENT_ENABLED=true 启动 unicorn-ai-mcp-client。
                5. 回滚时先停 client，再停 server；MCP 端点不保存业务状态。

                安全提示：MCP HTTP 端点默认无认证，生产环境必须置于网关或 Spring Security 之后。
                """;
    }

    @McpResource(uri = "unicorn://inventory/{sku}", name = "inventory-detail",
            description = "SKU 库存 JSON 资源", mimeType = "application/json")
    public String inventoryDetail(String sku) {
        var item = data.inventory(sku);
        return """
                {"sku":"%s","name":"%s","available":%d,"reserved":%d}
                """.formatted(item.sku(), item.name(), item.available(), item.reserved());
    }
}
