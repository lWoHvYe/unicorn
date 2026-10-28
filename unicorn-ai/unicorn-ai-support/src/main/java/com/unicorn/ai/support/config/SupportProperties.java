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

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

import java.math.BigDecimal;

@ConfigurationProperties("ai.support")
public record SupportProperties(
        @DefaultValue Demo demo,
        @DefaultValue("simple") String vectorStore,
        @DefaultValue("5") int topK,
        @DefaultValue("20") int candidateK,
        @DefaultValue("0.2") double similarityThreshold,
        @DefaultValue("800") int chunkSize,
        @DefaultValue("200") int minChunkSizeChars,
        @DefaultValue("8000") int maxInputChars,
        @DefaultValue("20") int memoryWindow,
        @DefaultValue("120000") long streamTimeoutMs,
        @DefaultValue("classpath:prompts/support-system.st") String systemPrompt,
        Storage storage,
        Pgvector pgvector,
        AsyncIngest asyncIngest,
        Query query,
        Mmr mmr,
        Rerank rerank,
        Pricing pricing,
        Approval approval,
        RateLimit rateLimit,
        Cache cache) {

    public record Demo(@DefaultValue("false") boolean seed, @DefaultValue("false") boolean model) {
    }

    public record Storage(@DefaultValue("${java.io.tmpdir}/unicorn-ai-support") String root) {
    }

    public record Pgvector(@DefaultValue("jdbc:postgresql://localhost:5432/vectordb") String url,
                           @DefaultValue("postgres") String username,
                           @DefaultValue("postgres") String password,
                           @DefaultValue("ai_support") String schemaName,
                           @DefaultValue("ai_support_chunk_embedding") String tableName,
                           @DefaultValue("1536") int dimensions) {
    }

    public record AsyncIngest(@DefaultValue("false") boolean enabled,
                              @DefaultValue("unicorn.ai.support") String exchange,
                              @DefaultValue("unicorn.ai.support.ingest") String queue,
                              @DefaultValue("ingest") String routingKey) {
    }

    public record Query(@DefaultValue("false") boolean rewriteEnabled,
                        @DefaultValue("2") int multiQueryCount) {
    }

    public record Mmr(@DefaultValue("false") boolean enabled,
                      @DefaultValue("0.7") double lambda) {
    }

    public record Rerank(@DefaultValue("false") boolean enabled,
                         @DefaultValue("gpt-4o-mini") String model) {
    }

    public record Pricing(@DefaultValue("0.00") double inputPer1kTokens,
                          @DefaultValue("0.00") double outputPer1kTokens,
                          @DefaultValue("USD") String currency) {
    }

    public record Approval(@DefaultValue("500.00") BigDecimal refundThreshold) {
    }

    public record RateLimit(@DefaultValue("60") int chatPerMinute,
                            @DefaultValue("100") int ingestPerHour,
                            @DefaultValue("60") int mcpPerMinute,
                            @DefaultValue("local") String store) {
    }

    public record Cache(@DefaultValue("local") String store) {
    }
}
