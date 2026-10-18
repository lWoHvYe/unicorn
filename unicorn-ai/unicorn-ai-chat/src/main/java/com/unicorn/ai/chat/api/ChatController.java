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

package com.unicorn.ai.chat.api;

import com.unicorn.ai.chat.model.AgentRun;
import com.unicorn.ai.chat.model.ArticlePlan;
import com.unicorn.ai.chat.model.ChatReply;
import com.unicorn.ai.chat.model.ChatRequest;
import com.unicorn.ai.chat.model.MemoryMessage;
import com.unicorn.ai.chat.model.StructuredRequest;
import com.unicorn.ai.chat.service.AgentService;
import com.unicorn.ai.chat.service.ChatService;
import com.unicorn.ai.chat.service.StructuredService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;

import java.util.List;

@RestController
@RequestMapping("/api/ai/chat")
@RequiredArgsConstructor
public class ChatController {

    private final ChatService chatService;
    private final StructuredService structuredService;
    private final AgentService agentService;

    @PostMapping
    public ChatReply chat(@RequestBody ChatRequest request) {
        return chatService.chat(request);
    }

    @PostMapping("/tools")
    public ChatReply tools(@RequestBody ChatRequest request,
                           @RequestParam(defaultValue = "demo") String tenant) {
        return chatService.chatWithTools(request, tenant);
    }

    @PostMapping("/structured")
    public ArticlePlan structured(@RequestBody StructuredRequest request) {
        return structuredService.plan(request);
    }

    @PostMapping("/agent")
    public AgentRun agent(@RequestBody ChatRequest request) {
        return agentService.run(request);
    }

    @GetMapping(value = "/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<String> stream(@RequestParam String message,
                               @RequestParam(defaultValue = "default") String conversationId) {
        return chatService.stream(new ChatRequest(message, conversationId, null));
    }

    @GetMapping("/memory/{conversationId}")
    public List<MemoryMessage> memory(@PathVariable String conversationId) {
        return chatService.memory(conversationId);
    }
}
