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

package com.unicorn.ai.support.api;

import com.unicorn.ai.support.chat.ChatDtos;
import com.unicorn.ai.support.chat.SupportChatService;
import com.unicorn.ai.support.security.AiPrincipal;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping("/api/ai/support/v1/sessions")
public class SupportChatController {

    private final SupportChatService chatService;
    private final com.unicorn.ai.support.config.SupportProperties properties;

    public SupportChatController(SupportChatService chatService,
                                 com.unicorn.ai.support.config.SupportProperties properties) {
        this.chatService = chatService;
        this.properties = properties;
    }

    @PostMapping
    public ChatDtos.SessionView create(@AuthenticationPrincipal AiPrincipal principal,
                                       @RequestBody(required = false) ChatDtos.CreateSessionRequest request) {
        var channel = request == null ? null : request.channel();
        var title = request == null ? null : request.title();
        var kbId = request == null ? null : request.kbId();
        return chatService.createSession(principal, channel, title, kbId);
    }

    @GetMapping("/{sessionId}")
    public ChatDtos.SessionView session(@AuthenticationPrincipal AiPrincipal principal,
                                        @PathVariable String sessionId) {
        return chatService.session(principal, sessionId);
    }

    @PostMapping("/{sessionId}/messages")
    public ChatDtos.ChatAnswer chat(@AuthenticationPrincipal AiPrincipal principal,
                                    @PathVariable String sessionId,
                                    @RequestBody ChatDtos.SendMessageRequest request,
                                    @RequestHeader(value = "Idempotency-Key", required = false) String idempotencyKey) {
        return chatService.chat(principal, sessionId, request.message(), idempotencyKey);
    }

    @PostMapping(value = "/{sessionId}/messages/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter stream(@AuthenticationPrincipal AiPrincipal principal,
                             @PathVariable String sessionId,
                             @RequestBody ChatDtos.SendMessageRequest request,
                             @RequestHeader(value = "Idempotency-Key", required = false) String idempotencyKey) {
        var emitter = new SseEmitter(properties.streamTimeoutMs());
        var disposable = chatService.stream(principal, sessionId, request.message(), idempotencyKey).subscribe(
                event -> {
                    try {
                        emitter.send(SseEmitter.event().name(event.event()).data(event.data()));
                    } catch (IOException exception) {
                        emitter.completeWithError(exception);
                    }
                },
                emitter::completeWithError,
                emitter::complete);
        emitter.onCompletion(disposable::dispose);
        emitter.onTimeout(() -> {
            disposable.dispose();
            emitter.complete();
        });
        emitter.onError(exception -> disposable.dispose());
        return emitter;
    }

    @GetMapping("/{sessionId}/messages")
    public List<ChatDtos.MessageView> messages(@AuthenticationPrincipal AiPrincipal principal,
                                               @PathVariable String sessionId) {
        return chatService.messages(principal, sessionId);
    }

    @PostMapping("/{sessionId}/feedback")
    public void feedback(@AuthenticationPrincipal AiPrincipal principal,
                         @PathVariable String sessionId,
                         @RequestBody ChatDtos.FeedbackRequest request) {
        chatService.feedback(principal, sessionId, request);
    }

    @PostMapping("/{sessionId}/close")
    public void close(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String sessionId) {
        chatService.close(principal, sessionId);
    }
}
