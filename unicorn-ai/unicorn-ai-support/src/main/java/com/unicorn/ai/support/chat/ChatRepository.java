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

package com.unicorn.ai.support.chat;

import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Optional;

@Repository
public class ChatRepository {

    private final JdbcTemplate jdbc;

    public ChatRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public void createSession(ChatRecords.Session session) {
        jdbc.update("""
                INSERT INTO ai_chat_session (id, tenant_id, user_id, channel, kb_id, title, status, summary)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """, session.id(), session.tenantId(), session.userId(), session.channel(), session.kbId(),
                session.title(), session.status(), session.summary());
    }

    public Optional<ChatRecords.Session> findSession(String tenantId, String id) {
        return jdbc.query("""
                        SELECT id, tenant_id, user_id, channel, kb_id, title, status, summary, created_at, updated_at
                        FROM ai_chat_session WHERE tenant_id = ? AND id = ?
                        """,
                (rs, rowNum) -> new ChatRecords.Session(
                        rs.getString("id"), rs.getString("tenant_id"), rs.getString("user_id"),
                        rs.getString("channel"), rs.getString("kb_id"), rs.getString("title"), rs.getString("status"),
                        rs.getString("summary"), toInstant(rs.getTimestamp("created_at")),
                        toInstant(rs.getTimestamp("updated_at"))),
                tenantId, id).stream().findFirst();
    }

    public void updateSummary(String tenantId, String id, String summary, String status) {
        jdbc.update("""
                UPDATE ai_chat_session
                SET summary = ?, status = ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND id = ?
                """, summary, status, tenantId, id);
    }

    public void saveMessage(ChatRecords.Message message) {
        jdbc.update("""
                INSERT INTO ai_chat_message
                    (id, session_id, tenant_id, role, content, citations_json, tool_calls_json, usage_json,
                     trace_id, status)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, message.id(), message.sessionId(), message.tenantId(), message.role(),
                message.content(), message.citationsJson(), message.toolCallsJson(), message.usageJson(),
                message.traceId(), message.status());
    }

    public List<ChatRecords.Message> recentMessages(String tenantId, String sessionId, int limit) {
        var rows = jdbc.query("""
                        SELECT id, session_id, tenant_id, role, content, citations_json, tool_calls_json,
                               usage_json, trace_id, status, created_at
                        FROM ai_chat_message
                        WHERE tenant_id = ? AND session_id = ?
                        ORDER BY created_at DESC
                        LIMIT ?
                        """,
                (rs, rowNum) -> new ChatRecords.Message(
                        rs.getString("id"), rs.getString("session_id"), rs.getString("tenant_id"),
                        rs.getString("role"), rs.getString("content"), rs.getString("citations_json"),
                        rs.getString("tool_calls_json"), rs.getString("usage_json"),
                        rs.getString("trace_id"), rs.getString("status"),
                        toInstant(rs.getTimestamp("created_at"))),
                tenantId, sessionId, limit);
        return rows.reversed();
    }

    public List<ChatRecords.Message> listMessages(String tenantId, String sessionId) {
        return jdbc.query("""
                        SELECT id, session_id, tenant_id, role, content, citations_json, tool_calls_json,
                               usage_json, trace_id, status, created_at
                        FROM ai_chat_message
                        WHERE tenant_id = ? AND session_id = ?
                        ORDER BY created_at
                        """,
                (rs, rowNum) -> new ChatRecords.Message(
                        rs.getString("id"), rs.getString("session_id"), rs.getString("tenant_id"),
                        rs.getString("role"), rs.getString("content"), rs.getString("citations_json"),
                        rs.getString("tool_calls_json"), rs.getString("usage_json"),
                        rs.getString("trace_id"), rs.getString("status"),
                        toInstant(rs.getTimestamp("created_at"))),
                tenantId, sessionId);
    }

    public boolean tryStartRequest(String tenantId, String sessionId, String requestId, String messageId) {
        try {
            jdbc.update("""
                    INSERT INTO ai_chat_request (tenant_id, session_id, request_id, message_id, status)
                    VALUES (?, ?, ?, ?, 'PROCESSING')
                    """, tenantId, sessionId, requestId, messageId);
            return true;
        } catch (DuplicateKeyException exception) {
            return false;
        }
    }

    public Optional<ChatRecords.Request> findRequest(String tenantId, String sessionId, String requestId) {
        return jdbc.query("""
                        SELECT tenant_id, session_id, request_id, message_id, status
                        FROM ai_chat_request WHERE tenant_id = ? AND session_id = ? AND request_id = ?
                        """,
                (rs, rowNum) -> new ChatRecords.Request(rs.getString("tenant_id"), rs.getString("session_id"),
                        rs.getString("request_id"), rs.getString("message_id"), rs.getString("status")),
                tenantId, sessionId, requestId).stream().findFirst();
    }

    public void finishRequest(String tenantId, String sessionId, String requestId, String status) {
        jdbc.update("""
                UPDATE ai_chat_request SET status = ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND session_id = ? AND request_id = ?
                """, status, tenantId, sessionId, requestId);
    }

    public Optional<ChatRecords.Message> findMessage(String tenantId, String id) {
        return jdbc.query("""
                        SELECT id, session_id, tenant_id, role, content, citations_json, tool_calls_json,
                               usage_json, trace_id, status, created_at
                        FROM ai_chat_message WHERE tenant_id = ? AND id = ?
                        """,
                (rs, rowNum) -> new ChatRecords.Message(
                        rs.getString("id"), rs.getString("session_id"), rs.getString("tenant_id"),
                        rs.getString("role"), rs.getString("content"), rs.getString("citations_json"),
                        rs.getString("tool_calls_json"), rs.getString("usage_json"), rs.getString("trace_id"),
                        rs.getString("status"), toInstant(rs.getTimestamp("created_at"))),
                tenantId, id).stream().findFirst();
    }

    public void saveFeedback(ChatRecords.Feedback feedback) {
        jdbc.update("""
                INSERT INTO ai_feedback (id, session_id, message_id, tenant_id, rating, comment)
                VALUES (?, ?, ?, ?, ?, ?)
                """, feedback.id(), feedback.sessionId(), feedback.messageId(), feedback.tenantId(),
                feedback.rating(), feedback.comment());
    }

    private Instant toInstant(Timestamp timestamp) {
        return timestamp == null ? null : timestamp.toInstant();
    }
}
