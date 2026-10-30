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
import com.unicorn.ai.support.retrieval.RetrievalCapture;
import com.unicorn.ai.support.retrieval.NoContextException;
import com.unicorn.ai.support.retrieval.CitationValidator;
import com.unicorn.ai.support.model.SupportModelInvoker;
import com.unicorn.ai.support.retrieval.RetrievalService;
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.tools.SupportToolFacade;
import com.unicorn.ai.support.observability.SupportMetrics;
import com.unicorn.ai.support.observability.UsageRepository;
import com.unicorn.ai.support.ratelimit.RateLimiterService;
import org.slf4j.MDC;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.client.advisor.MessageChatMemoryAdvisor;
import org.springframework.ai.chat.client.advisor.SafeGuardAdvisor;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.rag.advisor.RetrievalAugmentationAdvisor;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;
import tools.jackson.core.type.TypeReference;
import tools.jackson.databind.json.JsonMapper;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

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
    private final MessageChatMemoryAdvisor memoryAdvisor;
    private final RetrievalAugmentationAdvisor ragAdvisor;
    private final SafeGuardAdvisor safeGuardAdvisor;
    private final SupportModelInvoker modelInvoker;
    private final CitationValidator citationValidator;
    private final ChatClient fallbackChatClient;

    public SupportChatService(ChatClient chatClient, ChatRepository chatRepository,
                              RetrievalService retrievalService, SupportProperties properties,
                              SupportToolFacade toolFacade, UsageRepository usageRepository,
                              SupportMetrics metrics, RateLimiterService rateLimiter,
                              MessageChatMemoryAdvisor memoryAdvisor,
                              RetrievalAugmentationAdvisor ragAdvisor,
                              SafeGuardAdvisor safeGuardAdvisor,
                              SupportModelInvoker modelInvoker,
                              CitationValidator citationValidator,
                              @Qualifier("fallbackChatClient") ObjectProvider<ChatClient> fallbackChatClient) {
        this.chatClient = chatClient;
        this.chatRepository = chatRepository;
        this.retrievalService = retrievalService;
        this.properties = properties;
        this.toolFacade = toolFacade;
        this.usageRepository = usageRepository;
        this.metrics = metrics;
        this.rateLimiter = rateLimiter;
        this.memoryAdvisor = memoryAdvisor;
        this.ragAdvisor = ragAdvisor;
        this.safeGuardAdvisor = safeGuardAdvisor;
        this.modelInvoker = modelInvoker;
        this.citationValidator = citationValidator;
        this.fallbackChatClient = fallbackChatClient.getIfAvailable();
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
        var messageId = UUID.randomUUID().toString();
        var requestId = idempotencyKey == null || idempotencyKey.isBlank() ? null : idempotencyKey;
        if (requestId != null && !chatRepository.tryStartRequest(principal.tenantId(), sessionId, requestId, messageId)) {
            var existing = chatRepository.findRequest(principal.tenantId(), sessionId, requestId)
                    .orElseThrow(() -> new ChatRequestConflictException("聊天请求状态不可用"));
            if ("DONE".equals(existing.status())) {
                return chatRepository.findMessage(principal.tenantId(), existing.messageId())
                        .map(this::answerFrom)
                        .orElseThrow(() -> new ChatRequestConflictException("聊天请求已完成但响应缺失"));
            }
            throw new ChatRequestConflictException("相同幂等键的聊天请求正在处理中");
        }
        var userMessageId = UUID.randomUUID().toString();
        chatRepository.saveMessage(new ChatRecords.Message(userMessageId, sessionId, principal.tenantId(),
                "USER", message, null, null, null, traceId, "DONE", Instant.now()));

        var capture = new RetrievalCapture();
        var params = new java.util.HashMap<String, Object>();
        params.put("tenant", principal.tenantId());
        params.put("kbId", defaultKb(session));
        params.put("roles", principal.roles());
        params.put("capture", capture);
        params.put(ChatMemory.CONVERSATION_ID, conversationId(principal, sessionId));

        try {
            var response = modelInvoker.call(principal.tenantId(),
                    () -> callModel(chatClient, params, message, principal, sessionId, traceId, idempotencyKey),
                    fallbackChatClient == null ? null :
                            () -> callModel(fallbackChatClient, params, message, principal, sessionId,
                                    traceId, idempotencyKey));
            var retrieval = capture.require();
            var answer = response.getResult().getOutput().getText();
            var invalidCitations = citationValidator.invalidReferences(answer, retrieval.citations());
            var status = invalidCitations.isEmpty() ? "DONE" : "REFUSED";
            if (!invalidCitations.isEmpty()) {
                answer = citationValidator.refusalFor(invalidCitations);
            }
            var usage = usage(response);
            saveAssistant(principal, sessionId, messageId, answer, retrieval.citations(), usage, traceId, status);
            usageRepository.record(principal.tenantId(), usage.promptTokens(), usage.completionTokens(),
                    cost(usage));
            metrics.chat(principal.tenantId(), status, System.nanoTime() - started);
            finishRequest(principal, sessionId, requestId, "DONE");
            return new ChatDtos.ChatAnswer(messageId, answer, retrieval.citations(), usage, traceId, status);
        } catch (NoContextException exception) {
            var refusal = "资料中没有足够信息，无法基于当前知识库回答。请补充文档或转人工客服。";
            saveAssistant(principal, sessionId, messageId, refusal, List.of(),
                    ChatDtos.TokenUsage.empty(), traceId, "REFUSED");
            usageRepository.record(principal.tenantId(), 0, 0, 0);
            metrics.chat(principal.tenantId(), "REFUSED", System.nanoTime() - started);
            finishRequest(principal, sessionId, requestId, "DONE");
            return new ChatDtos.ChatAnswer(messageId, refusal, List.of(),
                    ChatDtos.TokenUsage.empty(), traceId, "REFUSED");
        } catch (RuntimeException exception) {
            saveAssistant(principal, sessionId, messageId, "AI 调用失败，请稍后重试。",
                    List.of(), ChatDtos.TokenUsage.empty(), traceId, "FAILED");
            metrics.chat(principal.tenantId(), "FAILED", System.nanoTime() - started);
            finishRequest(principal, sessionId, requestId, "FAILED");
            throw exception;
        }
    }

    public Flux<ChatDtos.StreamEvent> stream(AiPrincipal principal, String sessionId, String message) {
        validateMessage(message);
        rateLimiter.checkChat(principal.tenantId());
        var session = requireSession(principal, sessionId);
        var traceId = traceId();
        var messageId = UUID.randomUUID().toString();
        chatRepository.saveMessage(new ChatRecords.Message(UUID.randomUUID().toString(), sessionId,
                principal.tenantId(), "USER", message, null, null, null, traceId, "DONE", Instant.now()));

        final com.unicorn.ai.support.retrieval.RetrievalResult retrieval;
        try {
            retrieval = retrievalService.retrieve(principal.tenantId(), defaultKb(session), message, null,
                    principal.roles());
        } catch (RuntimeException exception) {
            throw exception;
        }
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
        var failed = new AtomicBoolean(false);
        var refused = new AtomicBoolean(false);
        var streamUsage = new java.util.concurrent.atomic.AtomicReference<>(ChatDtos.TokenUsage.empty());
        Sinks.Many<ChatDtos.StreamEvent> toolEvents = Sinks.many().unicast().onBackpressureBuffer();
        Consumer<ChatDtos.StreamEvent> eventSink = toolEvents::tryEmitNext;
        var capture = new RetrievalCapture();
        capture.set(retrieval);
        var params = new java.util.HashMap<String, Object>();
        params.put("tenant", principal.tenantId());
        params.put("kbId", defaultKb(session));
        params.put("roles", principal.roles());
        params.put("capture", capture);
        params.put(ChatMemory.CONVERSATION_ID, conversationId(principal, sessionId));
        var tokens = Flux.defer(() -> {
                    return chatClient.prompt()
                            .advisors(safeGuardAdvisor, memoryAdvisor, ragAdvisor)
                            .advisors(advisor -> advisor.params(params))
                            .user(message)
                            .tools(toolFacade)
                            .toolContext(toolContext(principal, sessionId, traceId, null, eventSink))
                            .stream()
                            .chatResponse()
                            .timeout(modelInvoker.timeout())
                            .doOnNext(response -> {
                                var usage = usage(response);
                                if (usage.totalTokens() > 0) {
                                    streamUsage.set(usage);
                                }
                                var text = response.getResult() == null || response.getResult().getOutput() == null
                                        ? null : response.getResult().getOutput().getText();
                                if (text != null) {
                                    answer.append(text);
                                }
                            })
                            .doOnComplete(() -> {
                                var finalAnswer = answer.toString();
                                var invalid = citationValidator.invalidReferences(finalAnswer, retrieval.citations());
                                var status = invalid.isEmpty() ? "DONE" : "REFUSED";
                                if (!invalid.isEmpty()) {
                                    refused.set(true);
                                    finalAnswer = citationValidator.refusalFor(invalid);
                                }
                                var usage = streamUsage.get();
                                saveAssistant(principal, sessionId, messageId, finalAnswer,
                                        retrieval.citations(), usage, traceId, status);
                                usageRepository.record(principal.tenantId(), usage.promptTokens(),
                                        usage.completionTokens(), cost(usage));
                            })
                            .mapNotNull(response -> {
                                var text = response.getResult() == null || response.getResult().getOutput() == null
                                        ? null : response.getResult().getOutput().getText();
                                return text == null || text.isEmpty() ? null
                                        : new ChatDtos.StreamEvent("token", java.util.Map.of("delta", text));
                            })
                            .doFinally(signal -> toolEvents.tryEmitComplete())
                            .onErrorResume(exception -> {
                                failed.set(true);
                                try {
                                    saveAssistant(principal, sessionId, messageId,
                                            "AI 调用失败，请稍后重试。", retrieval.citations(),
                                            ChatDtos.TokenUsage.empty(), traceId, "FAILED");
                                } catch (RuntimeException saveFailure) {
                                    // The stream still needs a terminal error event.
                                }
                                return Flux.just(new ChatDtos.StreamEvent("error",
                                        java.util.Map.of("code", "AI_CALL_FAILED",
                                                "message", "AI 调用失败，请稍后重试。")));
                            });
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
                        java.util.Map.of("messageId", messageId,
                                "status", failed.get() ? "FAILED" : refused.get() ? "REFUSED" : "DONE",
                                "usage", streamUsage.get()))));
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

    private void finishRequest(AiPrincipal principal, String sessionId, String requestId, String status) {
        if (requestId != null) {
            chatRepository.finishRequest(principal.tenantId(), sessionId, requestId, status);
        }
    }

    private ChatResponse callModel(ChatClient client, java.util.Map<String, Object> params, String message,
                                  AiPrincipal principal, String sessionId, String traceId,
                                  String idempotencyKey) {
        return client.prompt()
                .advisors(safeGuardAdvisor, memoryAdvisor, ragAdvisor)
                .advisors(advisor -> advisor.params(params))
                .user(message)
                .tools(toolFacade)
                .toolContext(toolContext(principal, sessionId, traceId, idempotencyKey))
                .call()
                .chatResponse();
    }

    private ChatDtos.ChatAnswer answerFrom(ChatRecords.Message message) {
        var citations = message.citationsJson() == null ? List.<Citation>of()
                : JSON.readValue(message.citationsJson(), new TypeReference<List<Citation>>() {
                });
        var usage = message.usageJson() == null ? ChatDtos.TokenUsage.empty()
                : JSON.readValue(message.usageJson(), ChatDtos.TokenUsage.class);
        return new ChatDtos.ChatAnswer(message.id(), message.content(), citations, usage,
                message.traceId(), message.status());
    }

    private ChatRecords.Session requireSession(AiPrincipal principal, String sessionId) {
        return chatRepository.findSession(principal.tenantId(), sessionId)
                .orElseThrow(() -> new java.util.NoSuchElementException("会话不存在"));
    }

    private String conversationId(AiPrincipal principal, String sessionId) {
        return principal.tenantId() + ":" + sessionId;
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
        return toolContext(principal, sessionId, traceId, idempotencyKey, null);
    }

    private java.util.Map<String, Object> toolContext(AiPrincipal principal, String sessionId,
                                                      String traceId, String idempotencyKey,
                                                      java.util.function.Consumer<ChatDtos.StreamEvent> eventSink) {
        var values = new java.util.HashMap<String, Object>();
        values.put("tenant", principal.tenantId());
        values.put("userId", principal.userId());
        values.put("roles", String.join(",", principal.roles()));
        values.put("sessionId", sessionId);
        values.put("idempotencyKey", idempotencyKey == null ? "" : idempotencyKey);
        values.put("traceId", traceId);
        if (eventSink != null) {
            values.put("eventSink", eventSink);
        }
        return values;
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

    private double cost(ChatDtos.TokenUsage usage) {
        var pricing = properties.pricing();
        return (usage.promptTokens() / 1000.0) * pricing.inputPer1kTokens()
                + (usage.completionTokens() / 1000.0) * pricing.outputPer1kTokens();
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
