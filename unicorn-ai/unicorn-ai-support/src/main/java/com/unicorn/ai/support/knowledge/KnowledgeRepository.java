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

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowCallbackHandler;
import org.springframework.stereotype.Repository;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Optional;

@Repository
public class KnowledgeRepository {

    private final JdbcTemplate jdbc;

    public KnowledgeRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public void createKnowledgeBase(KnowledgeRecords.KnowledgeBase kb) {
        jdbc.update("""
                INSERT INTO ai_knowledge_base (id, tenant_id, name, description, status)
                VALUES (?, ?, ?, ?, ?)
                """, kb.id(), kb.tenantId(), kb.name(), kb.description(), kb.status());
    }

    public List<KnowledgeRecords.KnowledgeBase> listKnowledgeBases(String tenantId) {
        return jdbc.query("""
                        SELECT id, tenant_id, name, description, status, created_at
                        FROM ai_knowledge_base WHERE tenant_id = ? ORDER BY created_at
                        """,
                (rs, rowNum) -> new KnowledgeRecords.KnowledgeBase(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("name"),
                        rs.getString("description"), rs.getString("status"), toInstant(rs.getTimestamp("created_at"))),
                tenantId);
    }

    public Optional<KnowledgeRecords.KnowledgeBase> findKnowledgeBase(String tenantId, String id) {
        return jdbc.query("""
                        SELECT id, tenant_id, name, description, status, created_at
                        FROM ai_knowledge_base WHERE tenant_id = ? AND id = ?
                        """,
                (rs, rowNum) -> new KnowledgeRecords.KnowledgeBase(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("name"),
                        rs.getString("description"), rs.getString("status"), toInstant(rs.getTimestamp("created_at"))),
                tenantId, id).stream().findFirst();
    }

    public void createDocument(KnowledgeRecords.Document document) {
        jdbc.update("""
                INSERT INTO ai_document
                    (id, tenant_id, kb_id, source, title, doc_type, acl_roles, content_hash, status, current_version_id)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, document.id(), document.tenantId(), document.kbId(), document.source(), document.title(),
                document.docType(), document.aclRoles(), document.contentHash(), document.status(),
                document.currentVersionId());
    }

    public Optional<KnowledgeRecords.Document> findDocument(String tenantId, String id) {
        return jdbc.query("""
                        SELECT id, tenant_id, kb_id, source, title, doc_type, acl_roles, content_hash,
                               status, current_version_id, created_at
                        FROM ai_document WHERE tenant_id = ? AND id = ?
                        """,
                (rs, rowNum) -> new KnowledgeRecords.Document(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("kb_id"),
                        rs.getString("source"), rs.getString("title"), rs.getString("doc_type"),
                        rs.getString("acl_roles"), rs.getString("content_hash"), rs.getString("status"),
                        rs.getString("current_version_id"), toInstant(rs.getTimestamp("created_at"))),
                tenantId, id).stream().findFirst();
    }

    public Optional<KnowledgeRecords.Document> findActiveByHash(String tenantId, String kbId, String hash) {
        return jdbc.query("""
                        SELECT id, tenant_id, kb_id, source, title, doc_type, acl_roles, content_hash,
                               status, current_version_id, created_at
                        FROM ai_document
                        WHERE tenant_id = ? AND kb_id = ? AND content_hash = ? AND status = 'ACTIVE'
                        """,
                (rs, rowNum) -> new KnowledgeRecords.Document(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("kb_id"),
                        rs.getString("source"), rs.getString("title"), rs.getString("doc_type"),
                        rs.getString("acl_roles"), rs.getString("content_hash"), rs.getString("status"),
                        rs.getString("current_version_id"), toInstant(rs.getTimestamp("created_at"))),
                tenantId, kbId, hash).stream().findFirst();
    }

    public java.util.Map<String, String> findDocumentStatuses(String tenantId,
                                                               java.util.Collection<String> documentIds) {
        if (documentIds == null || documentIds.isEmpty()) {
            return java.util.Map.of();
        }
        var ids = new java.util.ArrayList<>(new java.util.LinkedHashSet<>(documentIds));
        var placeholders = String.join(",", java.util.Collections.nCopies(ids.size(), "?"));
        var sql = "SELECT id, status FROM ai_document WHERE tenant_id = ? AND id IN (" + placeholders + ")";
        var args = new java.util.ArrayList<Object>();
        args.add(tenantId);
        args.addAll(ids);
        var result = new java.util.HashMap<String, String>();
        jdbc.query(sql, (RowCallbackHandler) rs -> result.put(rs.getString("id"), rs.getString("status")),
                args.toArray());
        return result;
    }

    public List<KnowledgeRecords.Document> listDocuments(String tenantId, String kbId) {
        return jdbc.query("""
                        SELECT id, tenant_id, kb_id, source, title, doc_type, acl_roles, content_hash,
                               status, current_version_id, created_at
                        FROM ai_document WHERE tenant_id = ? AND kb_id = ? ORDER BY created_at DESC
                        """,
                (rs, rowNum) -> new KnowledgeRecords.Document(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("kb_id"),
                        rs.getString("source"), rs.getString("title"), rs.getString("doc_type"),
                        rs.getString("acl_roles"), rs.getString("content_hash"), rs.getString("status"),
                        rs.getString("current_version_id"), toInstant(rs.getTimestamp("created_at"))),
                tenantId, kbId);
    }

    public int nextVersionNo(String documentId) {
        var value = jdbc.queryForObject(
                "SELECT COALESCE(MAX(version_no), 0) + 1 FROM ai_document_version WHERE document_id = ?",
                Integer.class, documentId);
        return value == null ? 1 : value;
    }

    public void createVersion(KnowledgeRecords.DocumentVersion version) {
        jdbc.update("""
                INSERT INTO ai_document_version
                    (id, tenant_id, document_id, version_no, content_hash, content_text, status,
                     chunk_count, error)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, version.id(), version.tenantId(), version.documentId(), version.versionNo(),
                version.contentHash(), version.contentText(), version.status(), version.chunkCount(),
                version.error());
    }

    public Optional<KnowledgeRecords.DocumentVersion> findVersion(String id) {
        return jdbc.query("""
                        SELECT id, tenant_id, document_id, version_no, content_hash, content_text, status,
                               chunk_count, error, created_at
                        FROM ai_document_version WHERE id = ?
                        """,
                (rs, rowNum) -> new KnowledgeRecords.DocumentVersion(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("document_id"),
                        rs.getInt("version_no"), rs.getString("content_hash"), rs.getString("content_text"),
                        rs.getString("status"), rs.getInt("chunk_count"), rs.getString("error"),
                        toInstant(rs.getTimestamp("created_at"))),
                id).stream().findFirst();
    }

    public void updateVersion(String id, String status, int chunkCount, String error) {
        jdbc.update("""
                UPDATE ai_document_version
                SET status = ?, chunk_count = ?, error = ?, updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                """, status, chunkCount, error, id);
    }

    public void updateDocumentVersion(String documentId, String versionId, String status) {
        jdbc.update("""
                UPDATE ai_document
                SET current_version_id = ?, status = ?, updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                """, versionId, status, documentId);
    }

    public int updateDocumentStatus(String tenantId, String documentId, String status) {
        return jdbc.update("""
                UPDATE ai_document SET status = ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND id = ?
                """, status, tenantId, documentId);
    }

    public void markDocumentDeleted(String tenantId, String documentId) {
        jdbc.update("""
                UPDATE ai_document SET status = 'DELETED', updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND id = ?
                """, tenantId, documentId);
    }

    public void saveChunks(List<KnowledgeRecords.Chunk> chunks) {
        jdbc.batchUpdate("""
                INSERT INTO ai_chunk
                    (id, tenant_id, kb_id, document_id, version_id, chunk_index, content, token_count, metadata_json)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, chunks.stream().map(chunk -> new Object[]{
                chunk.id(), chunk.tenantId(), chunk.kbId(), chunk.documentId(), chunk.versionId(),
                chunk.chunkIndex(), chunk.content(), chunk.tokenCount(), chunk.metadataJson()
        }).toList());
    }

    public List<KnowledgeRecords.Chunk> findChunksByDocument(String tenantId, String documentId, String versionId) {
        return jdbc.query("""
                        SELECT id, tenant_id, kb_id, document_id, version_id, chunk_index, content,
                               token_count, metadata_json
                        FROM ai_chunk
                        WHERE tenant_id = ? AND document_id = ? AND version_id = ?
                        ORDER BY chunk_index
                        """,
                (rs, rowNum) -> new KnowledgeRecords.Chunk(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("kb_id"),
                        rs.getString("document_id"), rs.getString("version_id"), rs.getInt("chunk_index"),
                        rs.getString("content"), rs.getInt("token_count"), rs.getString("metadata_json")),
                tenantId, documentId, versionId);
    }

    public List<KnowledgeRecords.Chunk> findChunksByDocument(String tenantId, String documentId) {
        return jdbc.query("""
                        SELECT id, tenant_id, kb_id, document_id, version_id, chunk_index, content,
                               token_count, metadata_json
                        FROM ai_chunk WHERE tenant_id = ? AND document_id = ?
                        ORDER BY version_id, chunk_index
                        """,
                (rs, rowNum) -> new KnowledgeRecords.Chunk(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("kb_id"),
                        rs.getString("document_id"), rs.getString("version_id"), rs.getInt("chunk_index"),
                        rs.getString("content"), rs.getInt("token_count"), rs.getString("metadata_json")),
                tenantId, documentId);
    }

    public void deleteChunksByDocument(String tenantId, String documentId, String versionId) {
        jdbc.update("DELETE FROM ai_chunk WHERE tenant_id = ? AND document_id = ? AND version_id = ?",
                tenantId, documentId, versionId);
    }

    public void deleteChunksByDocument(String tenantId, String documentId) {
        jdbc.update("DELETE FROM ai_chunk WHERE tenant_id = ? AND document_id = ?", tenantId, documentId);
    }

    public List<KnowledgeRecords.Chunk> lexicalSearch(String tenantId, String kbId, String query, int limit) {
        return jdbc.query("""
                        SELECT c.id, c.tenant_id, c.kb_id, c.document_id, c.version_id, c.chunk_index,
                               c.content, c.token_count, c.metadata_json
                        FROM ai_chunk c
                        JOIN ai_document d ON d.tenant_id = c.tenant_id
                            AND d.id = c.document_id AND d.current_version_id = c.version_id
                            AND d.status = 'ACTIVE'
                        WHERE c.tenant_id = ? AND c.kb_id = ? AND LOWER(c.content) LIKE LOWER(?)
                        ORDER BY c.chunk_index
                        LIMIT ?
                        """,
                (rs, rowNum) -> new KnowledgeRecords.Chunk(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("kb_id"),
                        rs.getString("document_id"), rs.getString("version_id"), rs.getInt("chunk_index"),
                        rs.getString("content"), rs.getInt("token_count"), rs.getString("metadata_json")),
                tenantId, kbId, "%" + query + "%", limit);
    }

    public void createJob(KnowledgeRecords.IngestJob job) {
        jdbc.update("""
                INSERT INTO ai_ingest_job
                    (id, tenant_id, document_id, version_id, status, attempts, error)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                """, job.id(), job.tenantId(), job.documentId(), job.versionId(), job.status(),
                job.attempts(), job.error());
    }

    public void updateJob(String id, String status, int attempts, String error) {
        jdbc.update("""
                UPDATE ai_ingest_job
                SET status = ?, attempts = ?, error = ?, updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                """, status, attempts, error, id);
    }

    public Optional<KnowledgeRecords.IngestJob> findJob(String tenantId, String id) {
        return jdbc.query("""
                        SELECT id, tenant_id, document_id, version_id, status, attempts, error,
                               created_at, updated_at
                        FROM ai_ingest_job WHERE tenant_id = ? AND id = ?
                        """,
                (rs, rowNum) -> new KnowledgeRecords.IngestJob(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("document_id"),
                        rs.getString("version_id"), rs.getString("status"), rs.getInt("attempts"),
                        rs.getString("error"), toInstant(rs.getTimestamp("created_at")),
                        toInstant(rs.getTimestamp("updated_at"))),
                tenantId, id).stream().findFirst();
    }

    public List<KnowledgeRecords.IngestJob> listJobs(String tenantId, String status) {
        var sql = """
                SELECT id, tenant_id, document_id, version_id, status, attempts, error,
                       created_at, updated_at
                FROM ai_ingest_job
                WHERE tenant_id = ?
                """;
        if (status != null && !status.isBlank()) {
            sql += " AND status = ? ";
        }
        sql += " ORDER BY created_at DESC LIMIT 100";
        var args = status == null || status.isBlank() ? new Object[]{tenantId} : new Object[]{tenantId, status};
        return jdbc.query(sql,
                (rs, rowNum) -> new KnowledgeRecords.IngestJob(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("document_id"),
                        rs.getString("version_id"), rs.getString("status"), rs.getInt("attempts"),
                        rs.getString("error"), toInstant(rs.getTimestamp("created_at")),
                        toInstant(rs.getTimestamp("updated_at"))),
                args);
    }

    private Instant toInstant(Timestamp timestamp) {
        return timestamp == null ? null : timestamp.toInstant();
    }
}
