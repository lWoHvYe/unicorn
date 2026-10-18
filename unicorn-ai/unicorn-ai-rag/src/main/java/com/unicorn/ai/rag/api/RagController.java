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

package com.unicorn.ai.rag.api;

import com.unicorn.ai.rag.async.AsyncIngestPublisher;
import com.unicorn.ai.rag.ingestion.DocumentIngestionService;
import com.unicorn.ai.rag.job.IngestJob;
import com.unicorn.ai.rag.job.IngestJobRepository;
import com.unicorn.ai.rag.model.AsyncIngestResponse;
import com.unicorn.ai.rag.model.IngestResult;
import com.unicorn.ai.rag.model.QueryExpansion;
import com.unicorn.ai.rag.model.RagAnswer;
import com.unicorn.ai.rag.model.RagQueryRequest;
import com.unicorn.ai.rag.model.RewriteResult;
import com.unicorn.ai.rag.model.SearchHit;
import com.unicorn.ai.rag.model.SourceStat;
import com.unicorn.ai.rag.model.TextIngestRequest;
import com.unicorn.ai.rag.service.RagService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@RestController
@RequestMapping("/api/ai/rag")
@RequiredArgsConstructor
public class RagController {

    private final DocumentIngestionService ingestionService;
    private final RagService ragService;
    private final IngestJobRepository ingestJobRepository;
    private final ObjectProvider<AsyncIngestPublisher> asyncIngestPublisher;

    @PostMapping(value = "/documents", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public IngestResult upload(@RequestPart("file") MultipartFile file,
                               @RequestParam(defaultValue = "demo") String tenant,
                               @RequestParam(defaultValue = "upload") String docType) {
        return ingestionService.ingest(file, tenant, docType);
    }

    @PostMapping("/documents/text")
    public IngestResult ingestText(@RequestBody TextIngestRequest request) {
        return ingestionService.ingestText(request);
    }

    /**
     * 异步入库：把任务投递到 RabbitMQ，立即返回 jobId；结果落 MySQL（infra profile）。
     */
    @PostMapping("/documents/text/async")
    public AsyncIngestResponse ingestTextAsync(@RequestBody TextIngestRequest request) {
        var publisher = asyncIngestPublisher.getIfAvailable();
        if (publisher == null) {
            throw new IllegalStateException("async ingest 未启用，请使用 --spring.profiles.active=infra");
        }
        return publisher.publish(request);
    }

    @GetMapping("/jobs/{jobId}")
    public ResponseEntity<IngestJob> job(@PathVariable String jobId) {
        return ingestJobRepository.find(jobId)
                .map(ResponseEntity::ok)
                .orElseGet(() -> ResponseEntity.notFound().build());
    }

    @PostMapping("/ingest/sample")
    public List<IngestResult> ingestSample() {
        return ingestionService.ingestClasspathDocs();
    }

    @GetMapping("/search")
    public List<SearchHit> search(@RequestParam("q") String question,
                                  @RequestParam(required = false) Integer topK,
                                  @RequestParam(required = false) String filter) {
        return ragService.search(question, topK, filter);
    }

    @PostMapping("/ask")
    public RagAnswer ask(@RequestBody RagQueryRequest request) {
        return ragService.ask(request);
    }

    @PostMapping("/ask/advisor")
    public RagAnswer askWithAdvisor(@RequestBody RagQueryRequest request) {
        return ragService.askWithAdvisor(request);
    }

    @PostMapping("/query/expand")
    public QueryExpansion expand(@RequestBody RagQueryRequest request) {
        return ragService.expand(request);
    }

    @PostMapping("/query/rewrite")
    public RewriteResult rewrite(@RequestBody RagQueryRequest request) {
        return ragService.rewrite(request);
    }

    @DeleteMapping("/documents")
    public void delete(@RequestParam String source) {
        ingestionService.deleteSource(source);
    }

    @GetMapping("/stats")
    public List<SourceStat> stats() {
        return ingestionService.stats();
    }
}
