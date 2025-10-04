/*
 *    Copyright (c) 2025.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.unicornopenai.rest;

import io.modelcontextprotocol.client.McpSyncClient;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.mcp.SyncMcpToolCallbackProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class MCPController {

    private final ChatClient.Builder chatClientBuilder;

    MCPController(ChatClient.Builder clientBuilder) {
        this.chatClientBuilder = clientBuilder;
    }

    @Autowired
    private List<McpSyncClient> mcpSyncClients;

    @Autowired
    private SyncMcpToolCallbackProvider toolCallbackProvider;

    @GetMapping("/mcp/weather")
    public String weather(@RequestParam(value = "city", defaultValue = "NY") String city) {
        var toolCallbacks = toolCallbackProvider.getToolCallbacks();
        var chatClient = chatClientBuilder.defaultToolCallbacks(toolCallbacks).build();
        var input = String.format("What is the weather in %s?", city);
        return chatClient.prompt(input).call().content();
    }
}
