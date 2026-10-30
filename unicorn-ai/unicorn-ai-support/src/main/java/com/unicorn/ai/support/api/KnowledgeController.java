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

package com.unicorn.ai.support.api;

import com.unicorn.ai.support.knowledge.DocumentIngestionService;
import com.unicorn.ai.support.knowledge.IngestResult;
import com.unicorn.ai.support.knowledge.KnowledgeDtos;
import com.unicorn.ai.support.knowledge.KnowledgeRecords;
import com.unicorn.ai.support.knowledge.KnowledgeRepository;
import com.unicorn.ai.support.knowledge.TextIngestRequest;
import com.unicorn.ai.support.audit.AuditService;
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.ratelimit.RateLimiterService;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
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
import java.util.UUID;

@RestController
@RequestMapping("/api/ai/support/v1")
public class KnowledgeController {

    private final KnowledgeRepository repository;
    private final DocumentIngestionService ingestionService;
    private final RateLimiterService rateLimiter;
    private final AuditService audit;

    public KnowledgeController(KnowledgeRepository repository, DocumentIngestionService ingestionService,
                               RateLimiterService rateLimiter, AuditService audit) {
        this.repository = repository;
        this.ingestionService = ingestionService;
        this.rateLimiter = rateLimiter;
        this.audit = audit;
    }

    @GetMapping("/knowledge-bases")
    public List<KnowledgeDtos.KnowledgeBaseView> knowledgeBases(@AuthenticationPrincipal AiPrincipal principal) {
        return repository.listKnowledgeBases(principal.tenantId()).stream()
                .map(kb -> new KnowledgeDtos.KnowledgeBaseView(kb.id(), kb.name(), kb.description(), kb.status()))
                .toList();
    }

    @PostMapping("/knowledge-bases")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public KnowledgeDtos.KnowledgeBaseView createKnowledgeBase(@AuthenticationPrincipal AiPrincipal principal,
                                                               @RequestBody KnowledgeDtos.CreateKnowledgeBaseRequest request) {
        if (request.name() == null || request.name().isBlank()) {
            throw new IllegalArgumentException("name 不能为空");
        }
        var kb = new KnowledgeRecords.KnowledgeBase(UUID.randomUUID().toString(), principal.tenantId(),
                request.name(), request.description(), "ACTIVE", java.time.Instant.now());
        repository.createKnowledgeBase(kb);
        return new KnowledgeDtos.KnowledgeBaseView(kb.id(), kb.name(), kb.description(), kb.status());
    }

    @PostMapping(value = "/knowledge-bases/{kbId}/documents", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public IngestResult upload(@AuthenticationPrincipal AiPrincipal principal,
                               @PathVariable String kbId,
                               @RequestPart("file") MultipartFile file,
                               @RequestParam(defaultValue = "upload") String docType,
                               @RequestParam(required = false) String aclRoles) {
        requireKb(principal, kbId);
        rateLimiter.checkIngest(principal.tenantId());
        return ingestionService.ingest(file, principal.tenantId(), kbId, docType, aclRoles);
    }

    @PostMapping("/knowledge-bases/{kbId}/documents/text")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public IngestResult ingestText(@AuthenticationPrincipal AiPrincipal principal,
                                   @PathVariable String kbId,
                                   @RequestBody TextIngestRequest request) {
        requireKb(principal, kbId);
        rateLimiter.checkIngest(principal.tenantId());
        return ingestionService.ingestText(principal.tenantId(), kbId, request);
    }

    @GetMapping("/knowledge-bases/{kbId}/documents")
    public List<KnowledgeDtos.DocumentView> documents(@AuthenticationPrincipal AiPrincipal principal,
                                                      @PathVariable String kbId) {
        requireKb(principal, kbId);
        return ingestionService.listDocuments(principal.tenantId(), kbId).stream()
                .map(document -> new KnowledgeDtos.DocumentView(document.id(), document.source(),
                        document.title(), document.docType(), document.status(), document.currentVersionId(),
                        document.createdAt().toString()))
                .toList();
    }

    @PostMapping("/documents/{documentId}/reindex")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public IngestResult reindex(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String documentId) {
        return ingestionService.reindex(principal.tenantId(), documentId);
    }

    @PostMapping("/documents/{documentId}/status")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public void updateStatus(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String documentId,
                             @RequestBody StatusRequest request) {
        if (request == null || request.status() == null || request.status().isBlank()) {
            throw new IllegalArgumentException("status 不能为空");
        }
        ingestionService.setStatus(principal.tenantId(), documentId, request.status());
        audit.record(principal, "KNOWLEDGE_STATUS", "ai_document", documentId,
                "{\"status\":\"" + request.status() + "\"}");
    }

    public record StatusRequest(String status) {
    }

    @DeleteMapping("/documents/{documentId}")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public void delete(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String documentId) {
        ingestionService.delete(principal.tenantId(), documentId);
    }

    @GetMapping("/jobs/{jobId}")
    public KnowledgeDtos.JobView job(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String jobId) {
        return ingestionService.job(principal.tenantId(), jobId)
                .map(this::toJob)
                .orElseThrow(() -> new java.util.NoSuchElementException("任务不存在"));
    }

    @GetMapping("/jobs")
    public List<KnowledgeDtos.JobView> jobs(@AuthenticationPrincipal AiPrincipal principal,
                                            @RequestParam(required = false) String status) {
        return ingestionService.listJobs(principal.tenantId(), status).stream().map(this::toJob).toList();
    }

    private void requireKb(AiPrincipal principal, String kbId) {
        repository.findKnowledgeBase(principal.tenantId(), kbId)
                .orElseThrow(() -> new java.util.NoSuchElementException("知识库不存在"));
    }

    private KnowledgeDtos.JobView toJob(KnowledgeRecords.IngestJob job) {
        return new KnowledgeDtos.JobView(job.id(), job.documentId(), job.versionId(), job.status(),
                job.attempts(), job.error(), job.createdAt().toString(), job.updatedAt().toString());
    }
}
