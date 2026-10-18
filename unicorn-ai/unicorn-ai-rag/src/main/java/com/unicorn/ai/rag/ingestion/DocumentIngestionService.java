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
import com.unicorn.ai.rag.model.IngestResult;
import com.unicorn.ai.rag.model.SourceStat;
import com.unicorn.ai.rag.model.TextIngestRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.document.Document;
import org.springframework.ai.reader.TextReader;
import org.springframework.ai.reader.markdown.MarkdownDocumentReader;
import org.springframework.ai.reader.markdown.config.MarkdownDocumentReaderConfig;
import org.springframework.ai.reader.tika.TikaDocumentReader;
import org.springframework.ai.transformer.splitter.TokenTextSplitter;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.ai.vectorstore.filter.FilterExpressionBuilder;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
@Service
@RequiredArgsConstructor
public class DocumentIngestionService {

    private final VectorStore vectorStore;
    private final RagProperties properties;
    private final Map<String, SourceStat> stats = new ConcurrentHashMap<>();

    public IngestResult ingest(MultipartFile file, String tenant, String docType) {
        if (file == null || file.isEmpty()) {
            throw new IllegalArgumentException("文件不能为空");
        }
        var source = StringUtils.hasText(file.getOriginalFilename()) ? file.getOriginalFilename() : "upload.bin";
        return ingestResource(file.getResource(), source, tenant, docType);
    }

    public IngestResult ingestText(TextIngestRequest request) {
        if (request == null || !StringUtils.hasText(request.text())) {
            throw new IllegalArgumentException("text 不能为空");
        }
        var source = StringUtils.hasText(request.source()) ? request.source() : "inline-" + UUID.randomUUID();
        var tenant = StringUtils.hasText(request.tenant()) ? request.tenant() : "demo";
        var docType = StringUtils.hasText(request.docType()) ? request.docType() : "text";
        var started = System.nanoTime();

        var documents = List.of(new Document(request.text()));
        var chunks = split(enrich(documents, source, tenant, docType));
        vectorStore.add(chunks);

        var result = new IngestResult(source, chunks.size(), elapsedMs(started), vectorStore.getName());
        stats.put(source, new SourceStat(source, chunks.size(), Instant.now()));
        return result;
    }

    public List<IngestResult> ingestClasspathDocs() {
        var resolver = new PathMatchingResourcePatternResolver();
        try {
            Resource[] resources = resolver.getResources("classpath:docs/*.md");
            return Arrays.stream(resources)
                    .sorted(Comparator.comparing(resource -> Objects.toString(resource.getFilename(), "")))
                    .map(resource -> ingestResource(resource,
                            "sample/" + Objects.toString(resource.getFilename(), "doc.md"), "sample", "markdown"))
                    .toList();
        } catch (IOException exception) {
            throw new UncheckedIOException(exception);
        }
    }

    public IngestResult ingestResource(Resource resource, String source, String tenant, String docType) {
        var started = System.nanoTime();
        var raw = read(resource, source);
        var chunks = split(enrich(raw, source, tenant, docType));
        vectorStore.add(chunks);

        var result = new IngestResult(source, chunks.size(), elapsedMs(started), vectorStore.getName());
        stats.put(source, new SourceStat(source, chunks.size(), Instant.now()));
        log.info("Ingested {} chunks from {} in {} ms", chunks.size(), source, result.elapsedMs());
        return result;
    }

    /**
     * 重新导入前先按 source 删除旧向量，保证 Demo 可重复执行。
     */
    public void deleteSource(String source) {
        if (!StringUtils.hasText(source)) {
            throw new IllegalArgumentException("source 不能为空");
        }
        vectorStore.delete(new FilterExpressionBuilder().eq("source", source).build());
        stats.remove(source);
    }

    public List<SourceStat> stats() {
        return stats.values().stream()
                .sorted(Comparator.comparing(SourceStat::source))
                .toList();
    }

    private List<Document> read(Resource resource, String source) {
        var lower = source.toLowerCase(Locale.ROOT);
        if (lower.endsWith(".md") || lower.endsWith(".markdown")) {
            var config = MarkdownDocumentReaderConfig.builder()
                    .withHorizontalRuleCreateDocument(true)
                    .withIncludeCodeBlock(true)
                    .withIncludeBlockquote(true)
                    .withAdditionalMetadata("source", source)
                    .build();
            return new MarkdownDocumentReader(resource, config).get();
        }
        if (lower.endsWith(".txt")) {
            var reader = new TextReader(resource);
            reader.getCustomMetadata().put("source", source);
            return reader.get();
        }
        // PDF/DOCX/PPTX/HTML 等统一交给 Tika；生产环境建议按格式选择更精确的 reader。
        return new TikaDocumentReader(resource).get();
    }

    private List<Document> enrich(List<Document> documents, String source, String tenant, String docType) {
        var ingestedAt = Instant.now().toString();
        return documents.stream()
                .map(document -> {
                    var metadata = new HashMap<String, Object>(document.getMetadata());
                    metadata.put("source", source);
                    metadata.put("tenant", tenant);
                    metadata.put("docType", docType);
                    metadata.put("ingestedAt", ingestedAt);
                    metadata.put("contentHash", sha256(document.getText()));
                    return new Document(document.getText(), metadata);
                })
                .toList();
    }

    private List<Document> split(List<Document> documents) {
        return TokenTextSplitter.builder()
                .withChunkSize(properties.chunkSize())
                .withMinChunkSizeChars(properties.minChunkSizeChars())
                .withMinChunkLengthToEmbed(10)
                .withMaxNumChunks(properties.maxChunks())
                .withKeepSeparator(true)
                // 中英文标点都作为候选断句点，避免中文长段落被硬切
                .withPunctuationMarks(List.of('。', '！', '？', '；', '\n', '.', '!', '?', ';', ':'))
                .build()
                .apply(documents);
    }

    private long elapsedMs(long started) {
        return (System.nanoTime() - started) / 1_000_000;
    }

    private String sha256(String text) {
        try {
            var digest = MessageDigest.getInstance("SHA-256");
            return HexFormat.of().formatHex(digest.digest(text.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException(exception);
        }
    }
}
