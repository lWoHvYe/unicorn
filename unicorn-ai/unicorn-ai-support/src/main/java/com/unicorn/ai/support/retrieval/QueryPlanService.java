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

package com.unicorn.ai.support.retrieval;

import com.unicorn.ai.support.config.SupportProperties;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

@Service
public class QueryPlanService {

    private final ChatClient chatClient;
    private final SupportProperties properties;

    public QueryPlanService(ChatClient chatClient, SupportProperties properties) {
        this.chatClient = chatClient;
        this.properties = properties;
    }

    public List<String> plan(String question) {
        var count = Math.max(1, Math.min(properties.query().multiQueryCount(), 4));
        if (!properties.query().rewriteEnabled() || count == 1) {
            return List.of(question);
        }
        try {
            var response = chatClient.prompt()
                    .system("""
                            你是检索查询规划器。把用户问题改写成最多 %d 条简洁、可独立检索的中文查询；
                            每行一条，不要编号，不要解释，不要回答问题。
                            """.formatted(count))
                    .user(question)
                    .call()
                    .content();
            var queries = new LinkedHashSet<String>();
            queries.add(question);
            if (response != null) {
                for (var line : response.split("\\R")) {
                    var value = line.replaceFirst("^[-*\\d.、\\s]+", "").trim();
                    if (!value.isBlank() && value.length() <= 300) {
                        queries.add(value);
                    }
                }
            }
            return new ArrayList<>(queries).subList(0, Math.min(count, queries.size()));
        } catch (RuntimeException exception) {
            return List.of(question);
        }
    }
}
