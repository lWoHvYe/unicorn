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

package com.unicorn.ai.support.async;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Repository
public class OutboxRepository {

    private final JdbcTemplate jdbc;

    public OutboxRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public void enqueue(String tenantId, String aggregateType, String aggregateId, String eventType,
                        String payloadJson) {
        jdbc.update("""
                INSERT INTO ai_outbox
                    (id, tenant_id, aggregate_type, aggregate_id, event_type, payload_json, status)
                VALUES (?, ?, ?, ?, ?, ?, 'PENDING')
                """, UUID.randomUUID().toString(), tenantId, aggregateType, aggregateId, eventType, payloadJson);
    }

    public List<OutboxEvent> claim(String owner, int limit) {
        var staleBefore = Timestamp.from(Instant.now().minusSeconds(300));
        jdbc.update("""
                UPDATE ai_outbox
                SET status = 'PUBLISHING', claimed_by = ?, claimed_at = CURRENT_TIMESTAMP,
                    updated_at = CURRENT_TIMESTAMP
                WHERE id IN (
                    SELECT id FROM ai_outbox
                    WHERE (status = 'PENDING' AND next_attempt_at <= CURRENT_TIMESTAMP)
                       OR (status = 'PUBLISHING' AND (claimed_at IS NULL OR claimed_at < ?))
                    ORDER BY created_at LIMIT ?
                )
                """, owner, staleBefore, limit);
        return jdbc.query("""
                SELECT id, tenant_id, aggregate_type, aggregate_id, event_type, payload_json, attempts
                FROM ai_outbox WHERE status = 'PUBLISHING' AND claimed_by = ?
                ORDER BY created_at LIMIT ?
                """, (rs, rowNum) -> new OutboxEvent(rs.getString("id"), rs.getString("tenant_id"),
                rs.getString("aggregate_type"), rs.getString("aggregate_id"), rs.getString("event_type"),
                rs.getString("payload_json"), rs.getInt("attempts")), owner, limit);
    }

    public void published(String id, String owner) {
        jdbc.update("""
                UPDATE ai_outbox SET status = 'PUBLISHED', claimed_by = NULL, claimed_at = NULL,
                    updated_at = CURRENT_TIMESTAMP WHERE id = ? AND claimed_by = ?
                """, id, owner);
    }

    public void failed(String id, String owner, int attempts) {
        var next = Timestamp.from(Instant.now().plusSeconds(Math.min(300, 5L * (attempts + 1))));
        jdbc.update("""
                UPDATE ai_outbox SET status = 'PENDING', attempts = attempts + 1,
                    claimed_by = NULL, claimed_at = NULL,
                    next_attempt_at = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ? AND claimed_by = ?
                """, next, id, owner);
    }

    public record OutboxEvent(String id, String tenantId, String aggregateType, String aggregateId,
                              String eventType, String payloadJson, int attempts) {
    }
}
