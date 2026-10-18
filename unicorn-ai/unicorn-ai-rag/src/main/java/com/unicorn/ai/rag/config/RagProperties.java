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

package com.unicorn.ai.rag.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

@ConfigurationProperties("demo.ai.rag")
public record RagProperties(
        @DefaultValue("simple") String vectorStore,
        @DefaultValue("0.35") double similarityThreshold,
        @DefaultValue("5") int topK,
        @DefaultValue("800") int chunkSize,
        @DefaultValue("350") int minChunkSizeChars,
        @DefaultValue("10000") int maxChunks,
        @DefaultValue("false") boolean ingestOnStartup,
        @DefaultValue("""
                你是一个严格基于给定资料回答问题的助手。
                只能使用 <context> 中的信息；如果资料不足，直接说明“资料中没有足够信息”，不要编造。
                回答后列出实际使用到的来源（source）。
                """) String systemPrompt,
        Redis redis) {

    public record Redis(
            @DefaultValue("localhost") String host,
            @DefaultValue("6379") int port,
            @DefaultValue("") String password,
            @DefaultValue("unicorn-ai-rag") String indexName,
            @DefaultValue("unicorn-ai-rag:") String prefix,
            @DefaultValue("1536") int dimensions) {
    }
}
