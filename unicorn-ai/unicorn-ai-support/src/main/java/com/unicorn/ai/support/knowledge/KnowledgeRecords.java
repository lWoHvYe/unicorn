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

import java.time.Instant;

public final class KnowledgeRecords {

    private KnowledgeRecords() {
    }

    public record KnowledgeBase(String id, String tenantId, String name, String description,
                                String status, Instant createdAt) {
    }

    public record Document(String id, String tenantId, String kbId, String source, String title,
                           String docType, String aclRoles, String contentHash, String status,
                           String currentVersionId, Instant createdAt) {
    }

    public record DocumentVersion(String id, String tenantId, String documentId, int versionNo,
                                  String contentHash, String contentText, String status, int chunkCount,
                                  String error, Instant createdAt) {
    }

    public record Chunk(String id, String tenantId, String kbId, String documentId, String versionId,
                        int chunkIndex, String content, int tokenCount, String metadataJson) {
    }

    public record IngestJob(String id, String tenantId, String documentId, String versionId,
                            String status, int attempts, String error, Instant createdAt, Instant updatedAt) {
    }
}
