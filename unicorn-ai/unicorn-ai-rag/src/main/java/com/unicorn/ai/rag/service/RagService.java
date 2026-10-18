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

package com.unicorn.ai.rag.service;

import com.unicorn.ai.rag.config.RagProperties;
import com.unicorn.ai.rag.model.QueryExpansion;
import com.unicorn.ai.rag.model.RagAnswer;
import com.unicorn.ai.rag.model.RagQueryRequest;
import com.unicorn.ai.rag.model.RewriteResult;
import com.unicorn.ai.rag.model.SearchHit;
import com.unicorn.ai.rag.model.TokenUsage;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.document.Document;
import org.springframework.ai.rag.Query;
import org.springframework.ai.rag.advisor.RetrievalAugmentationAdvisor;
import org.springframework.ai.rag.preretrieval.query.expansion.MultiQueryExpander;
import org.springframework.ai.rag.preretrieval.query.transformation.RewriteQueryTransformer;
import org.springframework.ai.rag.retrieval.search.VectorStoreDocumentRetriever;
import org.springframework.ai.vectorstore.SearchRequest;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RagService {

    private final ChatClient chatClient;
    private final VectorStore vectorStore;
    private final RagProperties properties;
    private final RetrievalAugmentationAdvisor retrievalAugmentationAdvisor;

    public List<SearchHit> search(String question, Integer topK, String filter) {
        return searchDocuments(question, topK, filter).stream().map(this::toHit).toList();
    }

    /**
     * 显式 RAG：先检索，再把 context 拼进 prompt。优点是来源、分数、trace 都完全可控。
     */
    public RagAnswer ask(RagQueryRequest request) {
        var question = requireQuestion(request);
        var documents = searchDocuments(question, request.topK(), request.filter());
        if (documents.isEmpty()) {
            return new RagAnswer("没有检索到相关资料，无法基于现有知识库回答。", List.of(), TokenUsage.empty(), "explicit");
        }

        var context = documents.stream()
                .map(this::formatDocument)
                .collect(Collectors.joining("\n\n---\n\n"));
        var userText = """
                请只根据下面的资料回答问题；资料不足时明确说明“资料中没有足够信息”。

                <context>
                %s
                </context>

                问题：%s
                """.formatted(context, question);

        ChatResponse response = chatClient.prompt()
                // 检索到的文档可能包含 {}，作为参数传入而不是模板本身
                .user(user -> user.text("{prompt}").param("prompt", userText))
                .call()
                .chatResponse();

        return new RagAnswer(
                response.getResult().getOutput().getText(),
                documents.stream().map(this::toHit).toList(),
                TokenUsage.from(response.getMetadata().getUsage()),
                "explicit");
    }

    /**
     * Advisor RAG：由 RetrievalAugmentationAdvisor 完成检索、上下文增强与拒绝策略。
     * 来源仍单独检索一次，便于返回 citations。
     */
    public RagAnswer askWithAdvisor(RagQueryRequest request) {
        var question = requireQuestion(request);
        var documents = searchDocuments(question, request.topK(), request.filter());

        var spec = chatClient.prompt()
                .advisors(retrievalAugmentationAdvisor)
                .user(user -> user.text("{question}").param("question", question));
        if (StringUtils.hasText(request.filter())) {
            spec = spec.advisors(advisors -> advisors.param(
                    VectorStoreDocumentRetriever.FILTER_EXPRESSION, request.filter()));
        }

        var clientResponse = spec.call().chatClientResponse();
        var response = clientResponse.chatResponse();
        return new RagAnswer(
                response.getResult().getOutput().getText(),
                documents.stream().map(this::toHit).toList(),
                TokenUsage.from(response.getMetadata().getUsage()),
                "retrieval-augmentation-advisor");
    }

    /**
     * Multi-Query：让模型把问题改写成多个语义变体，再合并检索结果。
     */
    public QueryExpansion expand(RagQueryRequest request) {
        var question = requireQuestion(request);
        var expander = MultiQueryExpander.builder()
                .chatClientBuilder(chatClient.mutate())
                .numberOfQueries(3)
                .includeOriginal(true)
                .build();
        List<Query> queries = expander.expand(new Query(question));
        var queryTexts = queries.stream().map(Query::text).toList();

        Map<String, Document> merged = new LinkedHashMap<>();
        for (var query : queries) {
            for (var document : searchDocuments(query.text(), request.topK(), request.filter())) {
                merged.putIfAbsent(document.getId(), document);
            }
        }
        return new QueryExpansion(question, queryTexts, merged.values().stream().map(this::toHit).toList());
    }

    /**
     * Query Rewrite：把口语化/含历史指代的问题改写成更适合向量检索的查询。
     */
    public RewriteResult rewrite(RagQueryRequest request) {
        var question = requireQuestion(request);
        var transformer = RewriteQueryTransformer.builder()
                .chatClientBuilder(chatClient.mutate())
                .targetSearchSystem("企业知识库向量检索")
                .build();
        var rewritten = transformer.transform(new Query(question));
        return new RewriteResult(question, rewritten.text());
    }

    private List<Document> searchDocuments(String question, Integer topK, String filter) {
        var builder = SearchRequest.builder()
                .query(question)
                .topK(topK == null ? properties.topK() : topK)
                .similarityThreshold(properties.similarityThreshold());
        if (StringUtils.hasText(filter)) {
            builder.filterExpression(filter);
        }
        return vectorStore.similaritySearch(builder.build());
    }

    private SearchHit toHit(Document document) {
        var source = Objects.toString(document.getMetadata().getOrDefault("source", "unknown"));
        return new SearchHit(source, truncate(document.getText(), 500), document.getScore(), document.getMetadata());
    }

    private String formatDocument(Document document) {
        var source = Objects.toString(document.getMetadata().getOrDefault("source", "unknown"));
        return "[source=" + source + "]\n" + document.getText();
    }

    private String requireQuestion(RagQueryRequest request) {
        if (request == null || !StringUtils.hasText(request.question())) {
            throw new IllegalArgumentException("question 不能为空");
        }
        return request.question();
    }

    private String truncate(String text, int maxLength) {
        if (text == null || text.length() <= maxLength) {
            return text;
        }
        return text.substring(0, maxLength) + "...";
    }
}
