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

package com.unicorn.ai.support.config;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ResourceLoader;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

@Configuration
public class SupportChatClientConfig {

    @Bean
    ChatClient supportChatClient(ChatClient.Builder builder, SupportProperties properties,
                                 ResourceLoader resourceLoader) throws IOException {
        return build(builder, properties, resourceLoader);
    }

    @Bean(name = "fallbackChatClient")
    @ConditionalOnProperty(name = "ai.support.model.fallback-model")
    ChatClient fallbackChatClient(ChatClient.Builder builder, SupportProperties properties,
                                  ResourceLoader resourceLoader) throws IOException {
        var fallback = builder.clone()
                .defaultOptions(OpenAiChatOptions.builder()
                        .model(properties.model().fallbackModel()));
        return build(fallback, properties, resourceLoader);
    }

    private ChatClient build(ChatClient.Builder builder, SupportProperties properties,
                             ResourceLoader resourceLoader) throws IOException {
        var resource = resourceLoader.getResource(properties.systemPrompt());
        var systemPrompt = resource.exists()
                ? resource.getContentAsString(StandardCharsets.UTF_8)
                : properties.systemPrompt();
        return builder.defaultSystem(systemPrompt).build();
    }
}
