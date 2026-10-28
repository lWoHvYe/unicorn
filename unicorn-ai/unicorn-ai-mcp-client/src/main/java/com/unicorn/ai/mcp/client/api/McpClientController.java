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

package com.unicorn.ai.mcp.client.api;

import com.unicorn.ai.mcp.client.model.ChatReply;
import com.unicorn.ai.mcp.client.model.ChatRequest;
import com.unicorn.ai.mcp.client.model.ToolInfo;
import com.unicorn.ai.mcp.client.config.McpClientProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.mcp.SyncMcpToolCallbackProvider;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/ai/mcp")
@RequiredArgsConstructor
public class McpClientController {

    private final ChatClient chatClient;
    private final ObjectProvider<SyncMcpToolCallbackProvider> mcpTools;
    private final McpClientProperties properties;

    @GetMapping("/tools")
    public List<ToolInfo> tools() {
        var provider = mcpTools.getIfAvailable();
        if (provider == null) {
            return List.of();
        }
        return Arrays.stream(provider.getToolCallbacks())
                .filter(this::allowed)
                .map(callback -> new ToolInfo(
                        callback.getToolDefinition().name(),
                        callback.getToolDefinition().description(),
                        callback.getToolDefinition().inputSchema()))
                .toList();
    }

    @GetMapping("/status")
    public Map<String, Object> status() {
        var provider = mcpTools.getIfAvailable();
        var count = provider == null ? 0 : (int) Arrays.stream(provider.getToolCallbacks())
                .filter(this::allowed).count();
        return Map.of("connected", provider != null, "toolCount", count);
    }

    @PostMapping("/chat")
    public ChatReply chat(@RequestBody ChatRequest request) {
        if (request == null || !StringUtils.hasText(request.message())) {
            throw new IllegalArgumentException("message 不能为空");
        }
        var answer = chatClient.prompt()
                .user(user -> user.text("{message}").param("message", request.message()))
                .call()
                .content();
        return new ChatReply(answer);
    }

    private boolean allowed(org.springframework.ai.tool.ToolCallback callback) {
        return properties.allowlist() == null || properties.allowlist().isEmpty()
                || properties.allowlist().contains(callback.getToolDefinition().name());
    }
}
