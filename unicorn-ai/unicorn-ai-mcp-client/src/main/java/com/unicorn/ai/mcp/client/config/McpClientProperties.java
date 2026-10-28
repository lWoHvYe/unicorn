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

package com.unicorn.ai.mcp.client.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

import java.util.List;

@ConfigurationProperties("demo.ai.mcp.client")
public record McpClientProperties(
        @DefaultValue("你是 Unicorn 的运维/订单助手，需要实时数据时必须调用 MCP 工具。") String systemPrompt,
        @DefaultValue("") String apiKey,
        @DefaultValue({"get_order", "refund_eligibility", "check_inventory", "search_products"}) List<String> allowlist) {
}
