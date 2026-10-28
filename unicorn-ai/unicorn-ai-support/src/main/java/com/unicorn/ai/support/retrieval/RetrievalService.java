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
import com.unicorn.ai.support.knowledge.KnowledgeRecords;
import com.unicorn.ai.support.knowledge.KnowledgeRepository;
import org.springframework.ai.document.Document;
import org.springframework.ai.vectorstore.SearchRequest;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.ai.vectorstore.filter.FilterExpressionBuilder;
import org.springframework.stereotype.Service;
import tools.jackson.databind.json.JsonMapper;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Hybrid retrieval: vector search + tenant-scoped lexical search, merged with reciprocal rank fusion.
 */
@Service
public class RetrievalService {

    private static final int RRF_K = 60;
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final VectorStore vectorStore;
    private final KnowledgeRepository knowledgeRepository;
    private final SupportProperties properties;

    public RetrievalService(VectorStore vectorStore, KnowledgeRepository knowledgeRepository,
                            SupportProperties properties) {
        this.vectorStore = vectorStore;
        this.knowledgeRepository = knowledgeRepository;
        this.properties = properties;
    }

    public RetrievalResult retrieve(String tenantId, String kbId, String query, Integer topK) {
        var limit = topK == null || topK <= 0 ? properties.topK() : Math.min(topK, 20);
        var candidateK = Math.max(properties.candidateK(), limit);

        var builder = new FilterExpressionBuilder();
        var filter = builder.and(
                builder.eq("tenant", tenantId),
                builder.eq("kbId", kbId)).build();

        var vectorDocs = vectorStore.similaritySearch(SearchRequest.builder()
                .query(query)
                .topK(candidateK)
                .similarityThreshold(properties.similarityThreshold())
                .filterExpression(filter)
                .build());

        var lexicalChunks = knowledgeRepository.lexicalSearch(tenantId, kbId, query, candidateK);
        var lexicalDocs = lexicalChunks.stream().map(this::toDocument).toList();

        var merged = rrf(vectorDocs, lexicalDocs);
        var selected = new ArrayList<Document>();
        var seenDocuments = new java.util.HashSet<String>();
        for (var document : merged) {
            var documentId = text(document, "documentId");
            if (documentId.isBlank() || seenDocuments.add(documentId)) {
                selected.add(document);
            }
            if (selected.size() >= limit) {
                break;
            }
        }

        var citations = selected.stream().map(this::toCitation).toList();
        return new RetrievalResult(selected, citations, !citations.isEmpty());
    }

    private List<Document> rrf(List<Document> vectorDocs, List<Document> lexicalDocs) {
        Map<String, Document> documents = new LinkedHashMap<>();
        Map<String, Double> scores = new LinkedHashMap<>();

        for (int i = 0; i < vectorDocs.size(); i++) {
            var document = vectorDocs.get(i);
            documents.put(document.getId(), document);
            scores.merge(document.getId(), 1.0 / (RRF_K + i + 1), Double::sum);
        }
        for (int i = 0; i < lexicalDocs.size(); i++) {
            var document = lexicalDocs.get(i);
            documents.putIfAbsent(document.getId(), document);
            scores.merge(document.getId(), 1.0 / (RRF_K + i + 1), Double::sum);
        }
        return documents.values().stream()
                .sorted(Comparator.comparingDouble((Document document) ->
                        scores.getOrDefault(document.getId(), 0.0)).reversed())
                .toList();
    }

    @SuppressWarnings("unchecked")
    private Document toDocument(KnowledgeRecords.Chunk chunk) {
        var metadata = new java.util.HashMap<String, Object>();
        if (chunk.metadataJson() != null && !chunk.metadataJson().isBlank()) {
            metadata.putAll(JSON.readValue(chunk.metadataJson(), Map.class));
        }
        metadata.put("tenant", chunk.tenantId());
        metadata.put("kbId", chunk.kbId());
        metadata.put("documentId", chunk.documentId());
        metadata.put("versionId", chunk.versionId());
        metadata.put("chunkId", chunk.id());
        metadata.putIfAbsent("source", chunk.documentId());
        metadata.putIfAbsent("title", chunk.documentId());
        metadata.put("chunkIndex", chunk.chunkIndex());
        return new Document(chunk.id(), chunk.content(), metadata);
    }

    private Citation toCitation(Document document) {
        return new Citation(
                text(document, "documentId"),
                text(document, "versionId"),
                document.getId(),
                text(document, "source"),
                text(document, "title"),
                truncate(document.getText(), 300),
                document.getScore() == null ? 0.0 : document.getScore());
    }

    private String text(Document document, String key) {
        return Objects.toString(document.getMetadata().getOrDefault(key, ""), "");
    }

    private String truncate(String text, int max) {
        return text == null || text.length() <= max ? text : text.substring(0, max) + "...";
    }
}
