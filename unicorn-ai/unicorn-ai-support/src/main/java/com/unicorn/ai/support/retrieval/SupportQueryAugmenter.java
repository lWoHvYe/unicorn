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

import org.springframework.ai.document.Document;
import org.springframework.ai.rag.Query;
import org.springframework.ai.rag.generation.augmentation.QueryAugmenter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class SupportQueryAugmenter implements QueryAugmenter {

    @Override
    public Query augment(Query query, List<Document> documents) {
        if (documents.isEmpty()) {
            throw new NoContextException("资料中没有足够信息");
        }
        var context = new StringBuilder();
        for (int i = 0; i < documents.size(); i++) {
            var document = documents.get(i);
            context.append("[C").append(i + 1).append("] source=")
                    .append(document.getMetadata().getOrDefault("source", "unknown")).append('\n')
                    .append(document.getText()).append("\n\n");
        }
        return query.mutate().text("""
                请只依据下面的资料回答用户问题；资料不足时明确说明“资料中没有足够信息”。
                引用资料时使用 [C1]、[C2] 标记。

                <context>
                %s
                </context>

                用户问题：%s
                """.formatted(context, query.text())).build();
    }
}
