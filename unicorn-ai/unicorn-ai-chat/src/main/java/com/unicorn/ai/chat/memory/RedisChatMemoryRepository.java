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

package com.unicorn.ai.chat.memory;

import org.springframework.ai.chat.memory.ChatMemoryRepository;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.MessageType;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.Objects;

/**
 * 基于现有 Redis 的轻量 ChatMemoryRepository。
 * 使用 List + TTL 存储，不依赖 RedisJSON/RediSearch，普通 Redis 或 Redis Stack 均可。
 * 仅持久化 user/assistant/system 三类文本消息；tool call 中间消息仍由 Spring AI 的窗口策略处理。
 */
public class RedisChatMemoryRepository implements ChatMemoryRepository {

    private final StringRedisTemplate redis;
    private final String keyPrefix;
    private final Duration ttl;

    public RedisChatMemoryRepository(StringRedisTemplate redis, String keyPrefix, Duration ttl) {
        this.redis = Objects.requireNonNull(redis, "redis");
        this.keyPrefix = Objects.requireNonNull(keyPrefix, "keyPrefix");
        this.ttl = Objects.requireNonNull(ttl, "ttl");
    }

    @Override
    public List<String> findConversationIds() {
        var keys = redis.keys(keyPrefix + "*");
        if (keys == null || keys.isEmpty()) {
            return List.of();
        }
        return keys.stream()
                .map(key -> key.substring(keyPrefix.length()))
                .sorted()
                .toList();
    }

    @Override
    public List<Message> findByConversationId(String conversationId) {
        var values = redis.opsForList().range(key(conversationId), 0, -1);
        if (values == null || values.isEmpty()) {
            return List.of();
        }
        return values.stream().map(RedisChatMemoryRepository::decode).toList();
    }

    @Override
    public void saveAll(String conversationId, List<Message> messages) {
        var key = key(conversationId);
        redis.delete(key);
        if (messages == null || messages.isEmpty()) {
            return;
        }
        redis.opsForList().rightPushAll(key, messages.stream().map(RedisChatMemoryRepository::encode).toList());
        redis.expire(key, ttl);
    }

    @Override
    public void deleteByConversationId(String conversationId) {
        redis.delete(key(conversationId));
    }

    private String key(String conversationId) {
        return keyPrefix + conversationId;
    }

    static String encode(Message message) {
        var text = message.getText() == null ? "" : message.getText();
        var payload = Base64.getEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8));
        return message.getMessageType().name() + "|" + payload;
    }

    static Message decode(String value) {
        var split = value.split("\\|", 2);
        if (split.length != 2) {
            return new UserMessage(value);
        }
        var text = new String(Base64.getDecoder().decode(split[1]), StandardCharsets.UTF_8);
        try {
            return switch (MessageType.valueOf(split[0])) {
                case USER -> new UserMessage(text);
                case ASSISTANT -> new AssistantMessage(text);
                case SYSTEM -> new SystemMessage(text);
                default -> new UserMessage(text);
            };
        } catch (IllegalArgumentException exception) {
            return new UserMessage(text);
        }
    }
}
