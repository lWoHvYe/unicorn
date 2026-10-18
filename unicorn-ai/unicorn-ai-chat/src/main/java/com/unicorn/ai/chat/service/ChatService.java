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

package com.unicorn.ai.chat.service;

import com.unicorn.ai.chat.model.ChatReply;
import com.unicorn.ai.chat.model.ChatRequest;
import com.unicorn.ai.chat.model.MemoryMessage;
import com.unicorn.ai.chat.model.TokenUsage;
import com.unicorn.ai.chat.tools.ShopTools;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.chat.prompt.ChatOptions;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class ChatService {

    private final ChatClient chatClient;
    private final ChatMemory chatMemory;
    private final ShopTools shopTools;

    public ChatReply chat(ChatRequest request) {
        var response = prepare(request).call().chatResponse();
        return reply(request, response);
    }

    public Flux<String> stream(ChatRequest request) {
        return prepare(request).stream().content();
    }

    /**
     * 通过 .tools(...) 把本地 @Tool 暴露给模型；Spring AI 2.0 会自动注册 ToolCallingAdvisor
     * 并完成多轮“模型请求 -> 执行工具 -> 回填结果”的循环。
     */
    public ChatReply chatWithTools(ChatRequest request, String tenant) {
        var response = prepare(request)
                .tools(shopTools)
                .toolContext(Map.of("tenant", tenant))
                .call()
                .chatResponse();
        return reply(request, response);
    }

    public List<MemoryMessage> memory(String conversationId) {
        return chatMemory.get(conversationId).stream()
                .map(message -> new MemoryMessage(message.getMessageType().getValue(), message.getText()))
                .toList();
    }

    private ChatClient.ChatClientRequestSpec prepare(ChatRequest request) {
        if (request.message() == null || request.message().isBlank()) {
            throw new IllegalArgumentException("message 不能为空");
        }
        var spec = chatClient.prompt()
                // 用户输入作为模板参数传入，避免内容中的 {} 被 StringTemplate 当成占位符
                .user(user -> user.text("{message}").param("message", request.message()))
                .advisors(advisors -> advisors.param(ChatMemory.CONVERSATION_ID, request.conversationIdOrDefault()));
        if (request.temperature() != null) {
            spec = spec.options(ChatOptions.builder().temperature(request.temperature()));
        }
        return spec;
    }

    private ChatReply reply(ChatRequest request, ChatResponse response) {
        var answer = response.getResult().getOutput().getText();
        return new ChatReply(
                request.conversationIdOrDefault(),
                answer,
                TokenUsage.from(response.getMetadata().getUsage()));
    }
}
