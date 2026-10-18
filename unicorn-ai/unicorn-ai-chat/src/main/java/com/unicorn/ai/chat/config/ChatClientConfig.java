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

package com.unicorn.ai.chat.config;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.client.advisor.MessageChatMemoryAdvisor;
import org.springframework.ai.chat.client.advisor.SimpleLoggerAdvisor;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.memory.MessageWindowChatMemory;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
@EnableConfigurationProperties(AiChatProperties.class)
public class ChatClientConfig {

    /**
     * 默认使用内存版滑动窗口；生产可换成 Redis/JDBC 的 ChatMemoryRepository。
     */
    @Bean
    ChatMemory chatMemory(AiChatProperties properties) {
        return MessageWindowChatMemory.builder()
                .maxMessages(properties.memoryWindow())
                .build();
    }

    /**
     * 记忆 advisor 放在工具循环之外（默认顺序），只持久化最终 user/assistant 消息；
     * SimpleLoggerAdvisor 用于观察请求/响应，需把日志级别调到 DEBUG 才能看到内容。
     */
    @Bean
    @Primary
    ChatClient chatClient(ChatClient.Builder builder, ChatMemory chatMemory, AiChatProperties properties) {
        return builder
                .defaultSystem(properties.systemPrompt())
                .defaultAdvisors(
                        MessageChatMemoryAdvisor.builder(chatMemory).build(),
                        new SimpleLoggerAdvisor())
                .build();
    }
}
