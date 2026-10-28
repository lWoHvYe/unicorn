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

package com.unicorn.ai.support.knowledge;

import com.unicorn.ai.support.async.AsyncIngestPublisher;
import com.unicorn.ai.support.config.SupportProperties;
import com.unicorn.ai.support.observability.SupportMetrics;
import com.unicorn.ai.support.util.Hashing;
import org.springframework.ai.document.Document;
import org.springframework.ai.reader.markdown.MarkdownDocumentReader;
import org.springframework.ai.reader.markdown.config.MarkdownDocumentReaderConfig;
import org.springframework.ai.reader.tika.TikaDocumentReader;
import org.springframework.ai.transformer.splitter.TokenTextSplitter;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

@Service
public class DocumentIngestionService {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final KnowledgeRepository repository;
    private final VectorStore vectorStore;
    private final SupportProperties properties;
    private final ObjectProvider<AsyncIngestPublisher> asyncPublisher;
    private final SupportMetrics metrics;

    public DocumentIngestionService(KnowledgeRepository repository, VectorStore vectorStore,
                                    SupportProperties properties,
                                    ObjectProvider<AsyncIngestPublisher> asyncPublisher,
                                    SupportMetrics metrics) {
        this.repository = repository;
        this.vectorStore = vectorStore;
        this.properties = properties;
        this.asyncPublisher = asyncPublisher;
        this.metrics = metrics;
    }

    public IngestResult ingest(MultipartFile file, String tenantId, String kbId, String docType) {
        if (file == null || file.isEmpty()) {
            throw new IllegalArgumentException("文件不能为空");
        }
        var source = StringUtils.hasText(file.getOriginalFilename()) ? file.getOriginalFilename() : "upload.bin";
        var text = read(file.getResource(), source);
        return create(tenantId, kbId, source, source, docType, text);
    }

    public IngestResult ingestText(String tenantId, String kbId, TextIngestRequest request) {
        if (request == null || !StringUtils.hasText(request.text())) {
            throw new IllegalArgumentException("text 不能为空");
        }
        var source = StringUtils.hasText(request.source()) ? request.source() : "inline-" + UUID.randomUUID();
        var title = StringUtils.hasText(request.title()) ? request.title() : source;
        var docType = StringUtils.hasText(request.docType()) ? request.docType() : "text";
        return create(tenantId, kbId, source, title, docType, request.text());
    }

    public IngestResult create(String tenantId, String kbId, String source, String title,
                               String docType, String text) {
        if (!StringUtils.hasText(text)) {
            throw new IllegalArgumentException("文档内容不能为空");
        }
        var hash = Hashing.sha256(text);
        var existing = repository.findActiveByHash(tenantId, kbId, hash);
        if (existing.isPresent()) {
            var document = existing.get();
            metrics.ingest(tenantId, "DUPLICATE");
            return new IngestResult(document.id(), null, document.source(), 0, "DUPLICATE", vectorStore.getName());
        }

        var documentId = UUID.randomUUID().toString();
        var versionId = UUID.randomUUID().toString();
        var jobId = UUID.randomUUID().toString();
        repository.createDocument(new KnowledgeRecords.Document(documentId, tenantId, kbId, source, title,
                docType, null, hash, "PROCESSING", null, Instant.now()));
        repository.createVersion(new KnowledgeRecords.DocumentVersion(versionId, documentId, 1, hash,
                text, "PENDING", 0, null, Instant.now()));
        repository.createJob(new KnowledgeRecords.IngestJob(jobId, tenantId, documentId, versionId,
                "PENDING", 0, null, Instant.now(), Instant.now()));

        var publisher = asyncPublisher.getIfAvailable();
        if (properties.asyncIngest().enabled() && publisher != null) {
            publisher.publish(jobId, tenantId, documentId, versionId);
            metrics.ingest(tenantId, "QUEUED");
            return new IngestResult(documentId, jobId, source, 0, "QUEUED", vectorStore.getName());
        }
        process(tenantId, documentId, versionId, jobId);
        return new IngestResult(documentId, jobId, source, repository.findVersion(versionId)
                .map(KnowledgeRecords.DocumentVersion::chunkCount).orElse(0), "COMPLETED", vectorStore.getName());
    }

    public void process(String tenantId, String documentId, String versionId, String jobId) {
        var attempts = repository.findJob(tenantId, jobId).map(KnowledgeRecords.IngestJob::attempts).orElse(0);
        try {
            repository.updateJob(jobId, "RUNNING", attempts, null);
            var document = repository.findDocument(tenantId, documentId)
                    .orElseThrow(() -> new IllegalArgumentException("文档不存在"));
            var version = repository.findVersion(versionId)
                    .orElseThrow(() -> new IllegalArgumentException("文档版本不存在"));
            var chunks = split(enrich(version.contentText(), tenantId, document.kbId(), documentId, versionId,
                    document.source()));
            vectorStore.add(chunks);
            repository.saveChunks(toRows(tenantId, documentId, versionId, chunks));
            var previousVersion = document.currentVersionId();
            repository.updateVersion(versionId, "ACTIVE", chunks.size(), null);
            repository.updateDocumentVersion(documentId, versionId, "ACTIVE");
            if (previousVersion != null && !previousVersion.equals(versionId)) {
                var oldChunks = repository.findChunksByDocument(tenantId, documentId, previousVersion);
                vectorStore.delete(oldChunks.stream().map(KnowledgeRecords.Chunk::id).toList());
                repository.deleteChunksByDocument(tenantId, documentId, previousVersion);
            }
            repository.updateJob(jobId, "COMPLETED", 0, null);
            metrics.ingest(tenantId, "COMPLETED");
        } catch (RuntimeException exception) {
            repository.updateVersion(versionId, "FAILED", 0, exception.getMessage());
            repository.updateJob(jobId, "FAILED", attempts + 1, exception.getMessage());
            metrics.ingest(tenantId, "FAILED");
            throw exception;
        }
    }

    public void delete(String tenantId, String documentId) {
        var document = repository.findDocument(tenantId, documentId)
                .orElseThrow(() -> new IllegalArgumentException("文档不存在"));
        if (document.currentVersionId() != null) {
            var chunks = repository.findChunksByDocument(tenantId, documentId, document.currentVersionId());
            vectorStore.delete(chunks.stream().map(KnowledgeRecords.Chunk::id).toList());
            repository.deleteChunksByDocument(tenantId, documentId, document.currentVersionId());
        }
        repository.markDocumentDeleted(tenantId, documentId);
    }

    public IngestResult reindex(String tenantId, String documentId) {
        var document = repository.findDocument(tenantId, documentId)
                .orElseThrow(() -> new IllegalArgumentException("文档不存在"));
        if (document.currentVersionId() == null) {
            throw new IllegalArgumentException("文档没有可重建的版本");
        }
        var old = repository.findVersion(document.currentVersionId())
                .orElseThrow(() -> new IllegalArgumentException("文档版本不存在"));
        var versionId = UUID.randomUUID().toString();
        var jobId = UUID.randomUUID().toString();
        repository.createVersion(new KnowledgeRecords.DocumentVersion(versionId, documentId,
                repository.nextVersionNo(documentId), old.contentHash(), old.contentText(), "PENDING", 0, null,
                Instant.now()));
        repository.createJob(new KnowledgeRecords.IngestJob(jobId, tenantId, documentId, versionId,
                "PENDING", 0, null, Instant.now(), Instant.now()));
        process(tenantId, documentId, versionId, jobId);
        return new IngestResult(documentId, jobId, document.source(), repository.findVersion(versionId)
                .map(KnowledgeRecords.DocumentVersion::chunkCount).orElse(0), "COMPLETED", vectorStore.getName());
    }

    public List<KnowledgeRecords.Document> listDocuments(String tenantId, String kbId) {
        return repository.listDocuments(tenantId, kbId);
    }

    public List<KnowledgeRecords.IngestJob> listJobs(String tenantId, String status) {
        return repository.listJobs(tenantId, status);
    }

    public java.util.Optional<KnowledgeRecords.IngestJob> job(String tenantId, String id) {
        return repository.findJob(tenantId, id);
    }

    private List<Document> enrich(String text, String tenantId, String kbId, String documentId, String versionId,
                                  String source) {
        var metadata = new HashMap<String, Object>();
        metadata.put("tenant", tenantId);
        metadata.put("kbId", kbId);
        metadata.put("documentId", documentId);
        metadata.put("versionId", versionId);
        metadata.put("source", source);
        metadata.put("title", source);
        return List.of(new Document(text, metadata));
    }

    private List<Document> split(List<Document> documents) {
        var splitter = TokenTextSplitter.builder()
                .withChunkSize(properties.chunkSize())
                .withMinChunkSizeChars(properties.minChunkSizeChars())
                .withMinChunkLengthToEmbed(10)
                .withMaxNumChunks(10_000)
                .withKeepSeparator(true)
                .withPunctuationMarks(List.of('。', '！', '？', '；', '\n', '.', '!', '?', ';', ':'))
                .build();
        var result = new ArrayList<Document>();
        for (var document : documents) {
            var chunks = splitter.apply(List.of(document));
            for (int i = 0; i < chunks.size(); i++) {
                var chunk = chunks.get(i);
                var metadata = new HashMap<>(chunk.getMetadata());
                metadata.put("chunkIndex", i);
                metadata.put("chunkId", UUID.randomUUID().toString());
                result.add(new Document((String) metadata.get("chunkId"), chunk.getText(), metadata));
            }
        }
        return result;
    }

    private List<KnowledgeRecords.Chunk> toRows(String tenantId, String documentId, String versionId,
                                                List<Document> documents) {
        var rows = new ArrayList<KnowledgeRecords.Chunk>();
        for (var document : documents) {
            var metadata = document.getMetadata();
            rows.add(new KnowledgeRecords.Chunk(
                    document.getId(),
                    tenantId,
                    (String) metadata.get("kbId"),
                    documentId,
                    versionId,
                    (Integer) metadata.getOrDefault("chunkIndex", 0),
                    document.getText(),
                    document.getText() == null ? 0 : document.getText().length() / 4,
                    writeJson(metadata)));
        }
        return rows;
    }

    private String read(Resource resource, String source) {
        var lower = source.toLowerCase(Locale.ROOT);
        try {
            if (lower.endsWith(".md") || lower.endsWith(".markdown")) {
                var config = MarkdownDocumentReaderConfig.builder()
                        .withHorizontalRuleCreateDocument(true)
                        .withIncludeCodeBlock(true)
                        .withIncludeBlockquote(true)
                        .build();
                return new MarkdownDocumentReader(resource, config).get().stream()
                        .map(Document::getText)
                        .collect(java.util.stream.Collectors.joining("\n\n"));
            }
            if (lower.endsWith(".txt")) {
                return new String(resource.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            }
            return new TikaDocumentReader(resource).get().stream()
                    .map(Document::getText)
                    .collect(java.util.stream.Collectors.joining("\n\n"));
        } catch (IOException exception) {
            throw new UncheckedIOException(exception);
        }
    }

    private String writeJson(Object value) {
        return JSON.writeValueAsString(value);
    }
}
