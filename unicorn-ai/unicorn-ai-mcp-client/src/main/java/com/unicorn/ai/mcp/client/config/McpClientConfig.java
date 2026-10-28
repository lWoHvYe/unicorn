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

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.mcp.SyncMcpToolCallbackProvider;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
@EnableConfigurationProperties(McpClientProperties.class)
public class McpClientConfig {

    /**
     * MCP 工具不会自动注册到 ChatClient，必须显式注入 provider。
     * 当 spring.ai.mcp.client.enabled=false 时 provider 不存在，ChatClient 仍可单独使用。
     */
    @Bean
    @Primary
    ChatClient chatClient(ChatClient.Builder builder,
                          ObjectProvider<SyncMcpToolCallbackProvider> mcpTools,
                          McpClientProperties properties) {
        var clientBuilder = builder.defaultSystem(properties.systemPrompt());
        mcpTools.ifAvailable(provider -> {
            var callbacks = java.util.Arrays.stream(provider.getToolCallbacks())
                    .filter(callback -> properties.allowlist() == null || properties.allowlist().isEmpty()
                            || properties.allowlist().contains(callback.getToolDefinition().name()))
                    .toArray(org.springframework.ai.tool.ToolCallback[]::new);
            clientBuilder.defaultTools((Object[]) callbacks);
        });
        return clientBuilder.build();
    }
}
