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

import com.unicorn.ai.chat.model.ArticlePlan;
import com.unicorn.ai.chat.model.StructuredRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class StructuredService {

    private final ChatClient chatClient;

    public ArticlePlan plan(StructuredRequest request) {
        if (request.topic() == null || request.topic().isBlank()) {
            throw new IllegalArgumentException("topic 不能为空");
        }
        var spec = chatClient.prompt()
                .user(user -> user.text("""
                                请为主题“{topic}”生成一份中文文章计划。
                                要求：
                                1. 3-5 个章节，每章 2-3 个要点；
                                2. 给出目标读者；
                                3. 给出 3-5 个关键词；
                                4. 只输出符合 ArticlePlan schema 的内容，不要额外解释。
                                """)
                        .param("topic", request.topic()))
                .advisors(advisors -> advisors.param(ChatMemory.CONVERSATION_ID, request.conversationIdOrDefault()));

        if (request.providerNative()) {
            // 走 provider 原生 JSON Schema + 本地 schema 校验；不支持 structured outputs 的兼容端点请传 false。
            return spec.call().entity(ArticlePlan.class,
                    entity -> entity.useProviderStructuredOutput().validateSchema());
        }
        return spec.call().entity(ArticlePlan.class);
    }
}
