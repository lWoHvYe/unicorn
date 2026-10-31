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
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
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
    private final com.unicorn.ai.support.retrieval.RetrievalCache retrievalCache;

    public DocumentIngestionService(KnowledgeRepository repository, VectorStore vectorStore,
                                    SupportProperties properties,
                                    ObjectProvider<AsyncIngestPublisher> asyncPublisher,
                                    SupportMetrics metrics,
                                    com.unicorn.ai.support.retrieval.RetrievalCache retrievalCache) {
        this.repository = repository;
        this.vectorStore = vectorStore;
        this.properties = properties;
        this.asyncPublisher = asyncPublisher;
        this.metrics = metrics;
        this.retrievalCache = retrievalCache;
    }

    @Transactional
    public IngestResult ingest(MultipartFile file, String tenantId, String kbId, String docType) {
        return ingest(file, tenantId, kbId, docType, null);
    }

    public IngestResult ingest(MultipartFile file, String tenantId, String kbId, String docType, String aclRoles) {
        if (file == null || file.isEmpty()) {
            throw new IllegalArgumentException("文件不能为空");
        }
        var source = StringUtils.hasText(file.getOriginalFilename()) ? file.getOriginalFilename() : "upload.bin";
        var text = read(file.getResource(), source);
        return create(tenantId, kbId, source, source, docType, text, aclRoles);
    }

    @Transactional
    public IngestResult ingestText(String tenantId, String kbId, TextIngestRequest request) {
        if (request == null || !StringUtils.hasText(request.text())) {
            throw new IllegalArgumentException("text 不能为空");
        }
        var source = StringUtils.hasText(request.source()) ? request.source() : "inline-" + UUID.randomUUID();
        var title = StringUtils.hasText(request.title()) ? request.title() : source;
        var docType = StringUtils.hasText(request.docType()) ? request.docType() : "text";
        return create(tenantId, kbId, source, title, docType, request.text(), request.aclRoles());
    }

    public IngestResult create(String tenantId, String kbId, String source, String title,
                               String docType, String text) {
        return create(tenantId, kbId, source, title, docType, text, null);
    }

    @Transactional
    public IngestResult create(String tenantId, String kbId, String source, String title,
                               String docType, String text, String aclRoles) {
        if (!StringUtils.hasText(text)) {
            throw new IllegalArgumentException("文档内容不能为空");
        }
        var hash = Hashing.sha256(text);
        var normalizedAcl = normalizeAcl(aclRoles);
        var existing = repository.findActiveByHash(tenantId, kbId, hash);
        if (existing.isPresent() && java.util.Objects.equals(normalizedAcl, existing.get().aclRoles())) {
            var document = existing.get();
            metrics.ingest(tenantId, "DUPLICATE");
            return new IngestResult(document.id(), null, document.source(), 0, "DUPLICATE", vectorStore.getName());
        }

        var documentId = UUID.randomUUID().toString();
        var versionId = UUID.randomUUID().toString();
        var jobId = UUID.randomUUID().toString();
        try {
            repository.createDocument(new KnowledgeRecords.Document(documentId, tenantId, kbId, source, title,
                    docType, normalizedAcl, hash, "PROCESSING", null, Instant.now()));
        } catch (DuplicateKeyException race) {
            var winner = repository.findActiveByHash(tenantId, kbId, hash);
            if (winner.isPresent()) {
                var document = winner.get();
                metrics.ingest(tenantId, "DUPLICATE");
                return new IngestResult(document.id(), null, document.source(), 0, "DUPLICATE", vectorStore.getName());
            }
            throw race;
        }
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
                    document.source(), document.title(), document.aclRoles()));
            try {
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
                retrievalCache.invalidate(tenantId, document.kbId());
                repository.updateJob(jobId, "COMPLETED", 0, null);
                metrics.ingest(tenantId, "COMPLETED");
            } catch (RuntimeException exception) {
                cleanupVersion(tenantId, documentId, versionId, chunks);
                throw exception;
            }
        } catch (RuntimeException exception) {
            repository.updateVersion(versionId, "FAILED", 0, exception.getMessage());
            repository.updateJob(jobId, "FAILED", attempts + 1, exception.getMessage());
            metrics.ingest(tenantId, "FAILED");
            throw exception;
        }
    }

    @Transactional
    public void delete(String tenantId, String documentId) {
        var document = repository.findDocument(tenantId, documentId)
                .orElseThrow(() -> new IllegalArgumentException("文档不存在"));
        var chunks = repository.findChunksByDocument(tenantId, documentId);
        vectorStore.delete(chunks.stream().map(KnowledgeRecords.Chunk::id).toList());
        repository.deleteChunksByDocument(tenantId, documentId);
        repository.markDocumentDeleted(tenantId, documentId);
        retrievalCache.invalidate(tenantId, document.kbId());
    }

    public void retire(String tenantId, String documentId) {
        var document = repository.findDocument(tenantId, documentId)
                .orElseThrow(() -> new IllegalArgumentException("文档不存在"));
        var chunks = repository.findChunksByDocument(tenantId, documentId);
        vectorStore.delete(chunks.stream().map(KnowledgeRecords.Chunk::id).toList());
        repository.deleteChunksByDocument(tenantId, documentId);
        repository.updateDocumentStatus(tenantId, documentId, "RETIRED");
        retrievalCache.invalidate(tenantId, document.kbId());
    }

    public void setStatus(String tenantId, String documentId, String status) {
        if (!"ACTIVE".equals(status) && !"RETIRED".equals(status) && !"REVIEW".equals(status)) {
            throw new IllegalArgumentException("status 只支持 ACTIVE/REVIEW/RETIRED");
        }
        if ("RETIRED".equals(status)) {
            retire(tenantId, documentId);
            return;
        }
        if (repository.updateDocumentStatus(tenantId, documentId, status) == 0) {
            throw new IllegalArgumentException("文档不存在");
        }
        repository.findDocument(tenantId, documentId)
                .ifPresent(document -> retrievalCache.invalidate(tenantId, document.kbId()));
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
                                  String source, String title, String aclRoles) {
        var metadata = new HashMap<String, Object>();
        metadata.put("tenant", tenantId);
        metadata.put("kbId", kbId);
        metadata.put("documentId", documentId);
        metadata.put("versionId", versionId);
        metadata.put("source", source);
        metadata.put("title", title);
        if (StringUtils.hasText(aclRoles)) {
            metadata.put("aclRoles", aclRoles);
        }
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
                var chunkId = UUID.nameUUIDFromBytes((metadata.get("documentId") + "|"
                        + metadata.get("versionId") + "|" + i).getBytes(java.nio.charset.StandardCharsets.UTF_8)).toString();
                metadata.put("chunkId", chunkId);
                result.add(new Document(chunkId, chunk.getText(), metadata));
            }
        }
        return result;
    }

    private void cleanupVersion(String tenantId, String documentId, String versionId, List<Document> chunks) {
        try {
            vectorStore.delete(chunks.stream().map(Document::getId).toList());
        } catch (RuntimeException ignored) {
            // The retry path also uses deterministic IDs; a stale vector is reported via the job.
        }
        try {
            repository.deleteChunksByDocument(tenantId, documentId, versionId);
        } catch (RuntimeException ignored) {
            // The version is still marked FAILED below.
        }
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

    private String normalizeAcl(String aclRoles) {
        if (!StringUtils.hasText(aclRoles)) {
            return null;
        }
        return java.util.Arrays.stream(aclRoles.split(","))
                .map(String::trim).filter(value -> !value.isBlank()).distinct()
                .collect(java.util.stream.Collectors.joining(","));
    }

    private String writeJson(Object value) {
        return JSON.writeValueAsString(value);
    }
}
