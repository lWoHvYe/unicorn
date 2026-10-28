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
import org.springframework.ai.document.Document;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Service
public class RerankService {

    private final ChatClient chatClient;
    private final SupportProperties properties;

    public RerankService(ChatClient chatClient, SupportProperties properties) {
        this.chatClient = chatClient;
        this.properties = properties;
    }

    public List<Document> rerank(String query, List<Document> documents) {
        if (!properties.rerank().enabled() || documents.size() < 2) {
            return documents;
        }
        try {
            var candidates = new StringBuilder();
            for (int i = 0; i < documents.size(); i++) {
                candidates.append('[').append(i + 1).append("] ")
                        .append(truncate(documents.get(i).getText(), 400)).append('\n');
            }
            var response = chatClient.prompt()
                    .system("""
                            你是检索重排器。只根据用户问题和候选片段的相关性排序；
                            只输出候选编号，逗号分隔，从最相关到最不相关，不要解释。
                            """)
                    .user("问题：" + query + "\n候选：\n" + candidates)
                    .call()
                    .content();
            if (response == null) {
                return documents;
            }
            Map<Integer, Integer> order = new LinkedHashMap<>();
            for (var token : response.split("[,，\\s]+")) {
                try {
                    var index = Integer.parseInt(token.replaceAll("[^0-9]", ""));
                    if (index >= 1 && index <= documents.size()) {
                        order.putIfAbsent(index - 1, order.size());
                    }
                } catch (NumberFormatException ignored) {
                    // Ignore free-form model output.
                }
            }
            var result = new ArrayList<>(documents);
            result.sort(Comparator.comparingInt(document -> order.getOrDefault(documents.indexOf(document),
                    Integer.MAX_VALUE)));
            return result;
        } catch (RuntimeException exception) {
            return documents;
        }
    }

    private String truncate(String text, int max) {
        return text == null || text.length() <= max ? text : text.substring(0, max) + "...";
    }
}
