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

package com.unicorn.ai.rag.ingestion;

import com.unicorn.ai.rag.config.RagProperties;
import com.unicorn.ai.rag.model.TextIngestRequest;
import org.junit.jupiter.api.Test;
import org.springframework.ai.document.Document;
import org.springframework.ai.vectorstore.SearchRequest;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.ai.vectorstore.filter.Filter;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DocumentIngestionServiceTest {

    private final CapturingVectorStore vectorStore = new CapturingVectorStore();
    private final RagProperties properties = new RagProperties(
            "simple", 0.35, 5, 80, 20, 100, false, "system", null);
    private final DocumentIngestionService service = new DocumentIngestionService(vectorStore, properties);

    @Test
    void ingestTextSplitsAndAddsMetadata() {
        var text = """
                Spring AI 提供 ChatClient、Advisors、Tool Calling 与 RAG 等能力。
                RAG 的关键是把文档切分成合适的 chunk，再写入向量库。
                TokenTextSplitter 支持按 token 切分，并允许配置中文标点作为断句点。
                检索时通过 similarityThreshold 与 topK 控制召回范围。
                """.repeat(8);

        var result = service.ingestText(new TextIngestRequest(text, "test/doc", "tenant-a", "faq"));

        assertTrue(result.chunks() > 0);
        assertFalse(vectorStore.documents.isEmpty());
        var first = vectorStore.documents.getFirst();
        assertEquals("test/doc", first.getMetadata().get("source"));
        assertEquals("tenant-a", first.getMetadata().get("tenant"));
        assertEquals("faq", first.getMetadata().get("docType"));
        assertTrue(first.getMetadata().containsKey("contentHash"));
    }

    static class CapturingVectorStore implements VectorStore {

        private final List<Document> documents = new ArrayList<>();

        @Override
        public void add(List<Document> documents) {
            this.documents.addAll(documents);
        }

        @Override
        public void delete(List<String> idList) {
            // no-op
        }

        @Override
        public void delete(Filter.Expression filterExpression) {
            // no-op
        }

        @Override
        public List<Document> similaritySearch(SearchRequest request) {
            return List.of();
        }
    }
}
