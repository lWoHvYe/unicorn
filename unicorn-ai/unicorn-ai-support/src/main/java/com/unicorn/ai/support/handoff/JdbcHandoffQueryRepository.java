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

package com.unicorn.ai.support.handoff;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.NoSuchElementException;

@Repository
public class JdbcHandoffQueryRepository {

    private final JdbcTemplate jdbc;

    public JdbcHandoffQueryRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public List<HandoffDtos.HandoffView> list(String tenantId, String status, int limit, int offset) {
        var sql = """
                SELECT id, session_id, requester_id, reason, status, assignee_id, priority, created_at
                FROM ai_handoff_ticket
                WHERE tenant_id = ? AND (? IS NULL OR status = ?)
                ORDER BY created_at DESC LIMIT ? OFFSET ?
                """;
        return jdbc.query(sql, (rs, rowNum) -> new HandoffDtos.HandoffView(
                        rs.getString("id"), rs.getString("session_id"), rs.getString("requester_id"),
                        rs.getString("reason"), rs.getString("status"), rs.getString("assignee_id"),
                        rs.getString("priority"), rs.getTimestamp("created_at").toInstant().toString()),
                tenantId, status, status, limit, offset);
    }

    public HandoffDtos.HandoffView assign(String tenantId, String id, String assigneeId) {
        var updated = jdbc.update("""
                UPDATE ai_handoff_ticket SET assignee_id = ?, status = 'ASSIGNED', updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND id = ? AND status IN ('OPEN', 'ASSIGNED')
                """, assigneeId, tenantId, id);
        if (updated == 0) {
            throw new NoSuchElementException("工单不存在或状态不可分配");
        }
        return get(tenantId, id);
    }

    public HandoffDtos.HandoffView close(String tenantId, String id) {
        var updated = jdbc.update("""
                UPDATE ai_handoff_ticket SET status = 'CLOSED', updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND id = ? AND status <> 'CLOSED'
                """, tenantId, id);
        if (updated == 0) {
            throw new NoSuchElementException("工单不存在或已关闭");
        }
        return get(tenantId, id);
    }

    private HandoffDtos.HandoffView get(String tenantId, String id) {
        return jdbc.query("""
                        SELECT id, session_id, requester_id, reason, status, assignee_id, priority, created_at
                        FROM ai_handoff_ticket WHERE tenant_id = ? AND id = ?
                        """,
                rs -> {
                    if (!rs.next()) {
                        throw new NoSuchElementException("工单不存在");
                    }
                    return new HandoffDtos.HandoffView(rs.getString("id"), rs.getString("session_id"),
                            rs.getString("requester_id"), rs.getString("reason"), rs.getString("status"),
                            rs.getString("assignee_id"), rs.getString("priority"),
                            rs.getTimestamp("created_at").toInstant().toString());
                }, tenantId, id);
    }
}
