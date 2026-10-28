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

package com.unicorn.ai.support.chat;

import com.unicorn.ai.support.config.SupportProperties;
import com.unicorn.ai.support.retrieval.Citation;
import com.unicorn.ai.support.retrieval.RetrievalResult;
import com.unicorn.ai.support.retrieval.RetrievalService;
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.tools.SupportToolFacade;
import com.unicorn.ai.support.observability.SupportMetrics;
import com.unicorn.ai.support.observability.UsageRepository;
import com.unicorn.ai.support.ratelimit.RateLimiterService;
import org.slf4j.MDC;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;
import com.unicorn.ai.support.tools.ToolEventContext;
import tools.jackson.core.type.TypeReference;
import tools.jackson.databind.json.JsonMapper;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
public class SupportChatService {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final ChatClient chatClient;
    private final ChatRepository chatRepository;
    private final RetrievalService retrievalService;
    private final SupportProperties properties;
    private final SupportToolFacade toolFacade;
    private final UsageRepository usageRepository;
    private final SupportMetrics metrics;
    private final RateLimiterService rateLimiter;

    public SupportChatService(ChatClient chatClient, ChatRepository chatRepository,
                              RetrievalService retrievalService, SupportProperties properties,
                              SupportToolFacade toolFacade, UsageRepository usageRepository,
                              SupportMetrics metrics, RateLimiterService rateLimiter) {
        this.chatClient = chatClient;
        this.chatRepository = chatRepository;
        this.retrievalService = retrievalService;
        this.properties = properties;
        this.toolFacade = toolFacade;
        this.usageRepository = usageRepository;
        this.metrics = metrics;
        this.rateLimiter = rateLimiter;
    }

    public ChatDtos.SessionView createSession(AiPrincipal principal, String channel, String title) {
        return createSession(principal, channel, title, null);
    }

    public ChatDtos.SessionView createSession(AiPrincipal principal, String channel, String title, String kbId) {
        var id = UUID.randomUUID().toString();
        var actualChannel = channel == null || channel.isBlank() ? "web" : channel;
        var actualTitle = title == null || title.isBlank() ? "新会话" : title;
        var actualKb = kbId == null || kbId.isBlank() ? "demo-kb" : kbId;
        chatRepository.createSession(new ChatRecords.Session(id, principal.tenantId(), principal.userId(),
                actualChannel, actualKb, actualTitle, "OPEN", null, Instant.now(), Instant.now()));
        return new ChatDtos.SessionView(id, principal.tenantId(), principal.userId(), actualChannel,
                actualKb, actualTitle, "OPEN", null, Instant.now().toString());
    }

    public ChatDtos.ChatAnswer chat(AiPrincipal principal, String sessionId, String message) {
        return chat(principal, sessionId, message, null);
    }

    public ChatDtos.ChatAnswer chat(AiPrincipal principal, String sessionId, String message,
                                    String idempotencyKey) {
        validateMessage(message);
        rateLimiter.checkChat(principal.tenantId());
        var session = requireSession(principal, sessionId);
        var started = System.nanoTime();
        var traceId = traceId();
        var history = history(principal.tenantId(), sessionId, session.summary());
        var userMessageId = UUID.randomUUID().toString();
        chatRepository.saveMessage(new ChatRecords.Message(userMessageId, sessionId, principal.tenantId(),
                "USER", message, null, null, null, traceId, "DONE", Instant.now()));

        var retrieval = retrievalService.retrieve(principal.tenantId(), defaultKb(session), message, null, principal.roles());
        var messageId = UUID.randomUUID().toString();
        if (!retrieval.answerable()) {
            var refusal = "资料中没有足够信息，无法基于当前知识库回答。请补充文档或转人工客服。";
            saveAssistant(principal, sessionId, messageId, refusal, retrieval.citations(),
                    ChatDtos.TokenUsage.empty(), traceId, "REFUSED");
            usageRepository.record(principal.tenantId(), 0, 0, 0);
            metrics.chat(principal.tenantId(), "REFUSED", System.nanoTime() - started);
            return new ChatDtos.ChatAnswer(messageId, refusal, retrieval.citations(),
                    ChatDtos.TokenUsage.empty(), traceId, "REFUSED");
        }

        try {
            var response = chatClient.prompt()
                    .messages(history)
                    .user(user -> user.text("{prompt}").param("prompt", prompt(message, retrieval)))
                    .tools(toolFacade)
                    .toolContext(toolContext(principal, sessionId, traceId, idempotencyKey))
                    .call()
                    .chatResponse();
            var answer = response.getResult().getOutput().getText();
            var usage = usage(response);
            saveAssistant(principal, sessionId, messageId, answer, retrieval.citations(), usage, traceId, "DONE");
            usageRepository.record(principal.tenantId(), usage.promptTokens(), usage.completionTokens(), 0);
            metrics.chat(principal.tenantId(), "DONE", System.nanoTime() - started);
            return new ChatDtos.ChatAnswer(messageId, answer, retrieval.citations(), usage, traceId, "DONE");
        } catch (RuntimeException exception) {
            saveAssistant(principal, sessionId, messageId, "AI 调用失败，请稍后重试。",
                    retrieval.citations(), ChatDtos.TokenUsage.empty(), traceId, "FAILED");
            metrics.chat(principal.tenantId(), "FAILED", System.nanoTime() - started);
            throw exception;
        }
    }

    public Flux<ChatDtos.StreamEvent> stream(AiPrincipal principal, String sessionId, String message) {
        validateMessage(message);
        rateLimiter.checkChat(principal.tenantId());
        var session = requireSession(principal, sessionId);
        var traceId = traceId();
        var history = history(principal.tenantId(), sessionId, session.summary());
        var messageId = UUID.randomUUID().toString();
        chatRepository.saveMessage(new ChatRecords.Message(UUID.randomUUID().toString(), sessionId,
                principal.tenantId(), "USER", message, null, null, null, traceId, "DONE", Instant.now()));

        var retrieval = retrievalService.retrieve(principal.tenantId(), defaultKb(session), message, null, principal.roles());
        if (!retrieval.answerable()) {
            var refusal = "资料中没有足够信息，无法基于当前知识库回答。请补充文档或转人工客服。";
            saveAssistant(principal, sessionId, messageId, refusal, retrieval.citations(),
                    ChatDtos.TokenUsage.empty(), traceId, "REFUSED");
            return Flux.just(
                    new ChatDtos.StreamEvent("meta", java.util.Map.of("messageId", messageId, "traceId", traceId)),
                    new ChatDtos.StreamEvent("token", java.util.Map.of("delta", refusal)),
                    new ChatDtos.StreamEvent("done", java.util.Map.of("messageId", messageId, "status", "REFUSED")));
        }

        var answer = new StringBuilder();
        Sinks.Many<ChatDtos.StreamEvent> toolEvents = Sinks.many().unicast().onBackpressureBuffer();
        var tokens = Flux.defer(() -> {
                    ToolEventContext.set(event -> toolEvents.tryEmitNext(event));
                    return chatClient.prompt()
                            .messages(history)
                            .user(user -> user.text("{prompt}").param("prompt", prompt(message, retrieval)))
                            .tools(toolFacade)
                            .toolContext(toolContext(principal, sessionId, traceId, null))
                            .stream()
                            .content()
                            .doOnNext(answer::append)
                            .doOnComplete(() -> saveAssistant(principal, sessionId, messageId, answer.toString(),
                                    retrieval.citations(), ChatDtos.TokenUsage.empty(), traceId, "DONE"))
                            .map(delta -> new ChatDtos.StreamEvent("token", java.util.Map.of("delta", delta)))
                            .doFinally(signal -> {
                                ToolEventContext.clear();
                                toolEvents.tryEmitComplete();
                            })
                            .onErrorResume(exception -> Flux.just(new ChatDtos.StreamEvent("error",
                                    java.util.Map.of("code", "AI_CALL_FAILED",
                                            "message", "AI 调用失败，请稍后重试。"))));
                });

        var prefix = new ArrayList<ChatDtos.StreamEvent>();
        prefix.add(new ChatDtos.StreamEvent("meta",
                java.util.Map.of("messageId", messageId, "traceId", traceId)));
        for (int i = 0; i < retrieval.citations().size(); i++) {
            prefix.add(new ChatDtos.StreamEvent("citation",
                    java.util.Map.of("index", i + 1, "citation", retrieval.citations().get(i))));
        }
        return Flux.concat(Flux.fromIterable(prefix), Flux.<ChatDtos.StreamEvent>merge(toolEvents.asFlux(), tokens),
                Flux.just(new ChatDtos.StreamEvent("done",
                        java.util.Map.of("messageId", messageId, "status", "DONE"))));
    }

    public ChatDtos.SessionView session(AiPrincipal principal, String sessionId) {
        var session = requireSession(principal, sessionId);
        return new ChatDtos.SessionView(session.id(), session.tenantId(), session.userId(), session.channel(),
                session.kbId(), session.title(), session.status(), session.summary(),
                session.createdAt().toString());
    }

    public List<ChatDtos.MessageView> messages(AiPrincipal principal, String sessionId) {
        requireSession(principal, sessionId);
        return chatRepository.listMessages(principal.tenantId(), sessionId).stream()
                .map(this::toView).toList();
    }

    public void feedback(AiPrincipal principal, String sessionId, ChatDtos.FeedbackRequest request) {
        requireSession(principal, sessionId);
        if (request.rating() < 1 || request.rating() > 5) {
            throw new IllegalArgumentException("rating 必须在 1-5 之间");
        }
        chatRepository.saveFeedback(new ChatRecords.Feedback(UUID.randomUUID().toString(), sessionId,
                request.messageId(), principal.tenantId(), request.rating(), request.comment()));
    }

    public void close(AiPrincipal principal, String sessionId) {
        requireSession(principal, sessionId);
        var messages = chatRepository.listMessages(principal.tenantId(), sessionId);
        var summary = summarize(messages);
        chatRepository.updateSummary(principal.tenantId(), sessionId, summary, "CLOSED");
    }

    private ChatRecords.Session requireSession(AiPrincipal principal, String sessionId) {
        return chatRepository.findSession(principal.tenantId(), sessionId)
                .orElseThrow(() -> new java.util.NoSuchElementException("会话不存在"));
    }

    private String defaultKb(ChatRecords.Session session) {
        return session.kbId() == null || session.kbId().isBlank() ? "demo-kb" : session.kbId();
    }

    private void validateMessage(String message) {
        if (message == null || message.isBlank()) {
            throw new IllegalArgumentException("message 不能为空");
        }
        if (message.length() > properties.maxInputChars()) {
            throw new IllegalArgumentException("message 超过最大长度 " + properties.maxInputChars());
        }
    }

    private java.util.Map<String, Object> toolContext(AiPrincipal principal, String sessionId,
                                                      String traceId, String idempotencyKey) {
        return java.util.Map.of(
                "tenant", principal.tenantId(),
                "userId", principal.userId(),
                "roles", String.join(",", principal.roles()),
                "sessionId", sessionId,
                "idempotencyKey", idempotencyKey == null ? "" : idempotencyKey,
                "traceId", traceId);
    }

    private String summarize(List<ChatRecords.Message> messages) {
        if (messages.isEmpty()) {
            return null;
        }
        var transcript = messages.stream()
                .map(message -> message.role() + ": " + message.content())
                .collect(java.util.stream.Collectors.joining("\n"));
        try {
            return chatClient.prompt()
                    .system("请把下面的客服会话压缩成不超过 300 字的中文摘要，保留客户目标、订单/商品信息、已执行动作和未决事项。")
                    .user(transcript)
                    .call()
                    .content();
        } catch (RuntimeException exception) {
            return transcript.length() <= 300 ? transcript : transcript.substring(0, 300);
        }
    }

    private List<Message> history(String tenantId, String sessionId, String summary) {
        var messages = new ArrayList<Message>();
        if (summary != null && !summary.isBlank()) {
            messages.add(new SystemMessage("此前会话摘要：" + summary));
        }
        for (var row : chatRepository.recentMessages(tenantId, sessionId, properties.memoryWindow())) {
            if ("USER".equals(row.role())) {
                messages.add(new UserMessage(row.content()));
            } else if ("ASSISTANT".equals(row.role())) {
                messages.add(new AssistantMessage(row.content()));
            }
        }
        return messages;
    }

    private String prompt(String question, RetrievalResult retrieval) {
        var context = new StringBuilder();
        for (int i = 0; i < retrieval.documents().size(); i++) {
            var document = retrieval.documents().get(i);
            context.append("[C").append(i + 1).append("] source=")
                    .append(document.getMetadata().getOrDefault("source", "unknown")).append('\n')
                    .append(document.getText()).append("\n\n");
        }
        return """
                请只依据下面的资料回答用户问题；资料不足时明确说明“资料中没有足够信息”。
                引用资料时使用 [C1]、[C2] 标记。

                <context>
                %s
                </context>

                用户问题：%s
                """.formatted(context, question);
    }

    private ChatDtos.TokenUsage usage(ChatResponse response) {
        var usage = response.getMetadata().getUsage();
        if (usage == null) {
            return ChatDtos.TokenUsage.empty();
        }
        return new ChatDtos.TokenUsage(usage.getPromptTokens(), usage.getCompletionTokens(),
                usage.getTotalTokens());
    }

    private void saveAssistant(AiPrincipal principal, String sessionId, String messageId, String answer,
                               List<Citation> citations, ChatDtos.TokenUsage usage, String traceId, String status) {
        chatRepository.saveMessage(new ChatRecords.Message(messageId, sessionId, principal.tenantId(),
                "ASSISTANT", answer, JSON.writeValueAsString(citations), null,
                JSON.writeValueAsString(usage), traceId, status, Instant.now()));
    }

    private ChatDtos.MessageView toView(ChatRecords.Message message) {
        var citations = message.citationsJson() == null ? List.<Citation>of()
                : JSON.readValue(message.citationsJson(), new TypeReference<List<Citation>>() {
                });
        var usage = message.usageJson() == null ? ChatDtos.TokenUsage.empty()
                : JSON.readValue(message.usageJson(), ChatDtos.TokenUsage.class);
        return new ChatDtos.MessageView(message.id(), message.role(), message.content(), citations,
                usage, message.traceId(), message.status(), message.createdAt().toString());
    }

    private String traceId() {
        var trace = MDC.get("traceId");
        return trace == null || trace.isBlank() ? UUID.randomUUID().toString() : trace;
    }
}
