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
import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.vectorstore.SearchRequest;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.ai.vectorstore.filter.FilterExpressionBuilder;
import org.springframework.stereotype.Service;
import tools.jackson.databind.json.JsonMapper;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Hybrid retrieval: vector + tenant-scoped lexical search, RRF fusion, document ACL,
 * optional MMR and optional LLM rerank.
 */
@Service
public class RetrievalService {

    private static final int RRF_K = 60;
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final VectorStore vectorStore;
    private final KnowledgeRepository knowledgeRepository;
    private final SupportProperties properties;
    private final QueryPlanService queryPlanService;
    private final EmbeddingModel embeddingModel;
    private final RerankService rerankService;
    private final RetrievalCache cache;

    public RetrievalService(VectorStore vectorStore, KnowledgeRepository knowledgeRepository,
                            SupportProperties properties, QueryPlanService queryPlanService,
                            EmbeddingModel embeddingModel, RerankService rerankService,
                            RetrievalCache cache) {
        this.vectorStore = vectorStore;
        this.knowledgeRepository = knowledgeRepository;
        this.properties = properties;
        this.queryPlanService = queryPlanService;
        this.embeddingModel = embeddingModel;
        this.rerankService = rerankService;
        this.cache = cache;
    }

    public RetrievalResult retrieve(String tenantId, String kbId, String query, Integer topK) {
        return retrieve(tenantId, kbId, query, topK, Set.of());
    }

    public RetrievalResult retrieve(String tenantId, String kbId, String query, Integer topK, Set<String> roles) {
        var limit = topK == null || topK <= 0 ? properties.topK() : Math.min(topK, 20);
        var cached = cache.get(tenantId, kbId, query, limit, roles);
        if (cached.isPresent()) {
            return cached.get();
        }
        // ACL is applied after vector/lexical retrieval; fetch extra candidates so private
        // documents cannot crowd every public result out of the topK.
        var candidateK = Math.max(properties.candidateK(), limit * 5);

        var rankedLists = new ArrayList<List<Document>>();
        for (var plannedQuery : queryPlanService.plan(query)) {
            rankedLists.add(search(tenantId, kbId, plannedQuery, candidateK, roles));
        }
        var merged = fuse(rankedLists);
        var selected = select(merged, limit, query);
        var documentIds = selected.stream()
                .map(document -> document.getMetadata().get("documentId"))
                .filter(java.util.Objects::nonNull)
                .map(Object::toString)
                .distinct()
                .toList();
        var statuses = knowledgeRepository.findDocumentStatuses(tenantId, documentIds);
        selected = selected.stream()
                .filter(document -> "ACTIVE".equals(statuses.get(
                        String.valueOf(document.getMetadata().get("documentId")))))
                .toList();
        if (properties.rerank().enabled()) {
            selected = rerankService.rerank(query, selected);
        }

        var citations = selected.stream().map(this::toCitation).toList();
        var result = new RetrievalResult(selected, citations, !citations.isEmpty());
        cache.put(tenantId, kbId, query, limit, roles, result);
        return result;
    }

    private List<Document> search(String tenantId, String kbId, String query, int candidateK, Set<String> roles) {
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
        var lexicalDocs = knowledgeRepository.lexicalSearch(tenantId, kbId, query, candidateK).stream()
                .map(this::toDocument)
                .toList();

        vectorDocs = vectorDocs.stream().filter(document -> allowed(document, roles)).toList();
        lexicalDocs = lexicalDocs.stream().filter(document -> allowed(document, roles)).toList();
        return rrf(vectorDocs, lexicalDocs);
    }

    private List<Document> fuse(List<List<Document>> lists) {
        if (lists.size() == 1) {
            return lists.getFirst();
        }
        Map<String, Document> documents = new LinkedHashMap<>();
        Map<String, Double> scores = new HashMap<>();
        for (var list : lists) {
            for (int i = 0; i < list.size(); i++) {
                var document = list.get(i);
                documents.putIfAbsent(document.getId(), document);
                scores.merge(document.getId(), 1.0 / (RRF_K + i + 1), Double::sum);
            }
        }
        return documents.values().stream()
                .sorted(Comparator.comparingDouble((Document document) ->
                        scores.getOrDefault(document.getId(), 0.0)).reversed())
                .toList();
    }

    private List<Document> select(List<Document> candidates, int limit, String query) {
        var ordered = properties.mmr().enabled() ? mmr(candidates, limit, query) : candidates;
        var selected = new ArrayList<Document>();
        var seenDocuments = new LinkedHashSet<String>();
        for (var document : ordered) {
            if (seenDocuments.add(text(document, "documentId"))) {
                selected.add(document);
            }
            if (selected.size() >= limit) {
                break;
            }
        }
        return selected;
    }

    private List<Document> mmr(List<Document> candidates, int limit, String query) {
        if (candidates.size() <= 1) {
            return candidates;
        }
        try {
            var texts = new ArrayList<String>(candidates.size() + 1);
            texts.add(query);
            candidates.forEach(candidate -> texts.add(candidate.getText()));
            var embeddings = embeddingModel.embed(texts);
            var queryVector = embeddings.getFirst();
            var vectors = new HashMap<String, float[]>();
            for (int i = 0; i < candidates.size(); i++) {
                vectors.put(candidates.get(i).getId(), embeddings.get(i + 1));
            }
            var remaining = new ArrayList<>(candidates);
            var selected = new ArrayList<Document>();
            while (selected.size() < limit && !remaining.isEmpty()) {
                Document best = null;
                double bestScore = Double.NEGATIVE_INFINITY;
                for (var candidate : remaining) {
                    var relevance = cosine(queryVector, vectors.get(candidate.getId()));
                    var redundancy = selected.stream()
                            .mapToDouble(chosen -> cosine(vectors.get(candidate.getId()), vectors.get(chosen.getId())))
                            .max().orElse(0.0);
                    var score = properties.mmr().lambda() * relevance
                            - (1 - properties.mmr().lambda()) * redundancy;
                    if (score > bestScore) {
                        bestScore = score;
                        best = candidate;
                    }
                }
                if (best == null) {
                    break;
                }
                selected.add(best);
                remaining.remove(best);
            }
            return selected;
        } catch (RuntimeException exception) {
            return candidates;
        }
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

    private boolean allowed(Document document, Set<String> roles) {
        var acl = text(document, "aclRoles");
        if (acl.isBlank()) {
            return true;
        }
        if (roles == null || roles.isEmpty()) {
            return false;
        }
        if (roles.contains("TENANT_ADMIN")) {
            return true;
        }
        var effective = new java.util.HashSet<>(roles);
        if (effective.contains("SUPPORT_MANAGER")) {
            effective.add("SUPPORT_AGENT");
        }
        return java.util.Arrays.stream(acl.split(","))
                .map(String::trim)
                .anyMatch(effective::contains);
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

    private double cosine(float[] left, float[] right) {
        if (left == null || right == null || left.length != right.length) {
            return 0.0;
        }
        double dot = 0;
        double leftNorm = 0;
        double rightNorm = 0;
        for (int i = 0; i < left.length; i++) {
            dot += left[i] * right[i];
            leftNorm += left[i] * left[i];
            rightNorm += right[i] * right[i];
        }
        return leftNorm == 0 || rightNorm == 0 ? 0.0 : dot / (Math.sqrt(leftNorm) * Math.sqrt(rightNorm));
    }

    private String truncate(String text, int max) {
        return text == null || text.length() <= max ? text : text.substring(0, max) + "...";
    }
}
