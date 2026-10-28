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

package com.unicorn.ai.support.tools;

import com.unicorn.ai.tools.model.ApprovalStatus;
import com.unicorn.ai.tools.model.ToolInvocation;
import com.unicorn.ai.tools.model.ToolStatus;
import com.unicorn.ai.tools.policy.ToolInvocationRepository;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import tools.jackson.databind.json.JsonMapper;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Repository
public class JdbcToolInvocationRepository implements ToolInvocationRepository {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final JdbcTemplate jdbc;

    public JdbcToolInvocationRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public Optional<ToolInvocation> findByIdempotencyKey(String tenantId, String idempotencyKey) {
        return jdbc.query("""
                        SELECT * FROM ai_tool_invocation WHERE tenant_id = ? AND idempotency_key = ?
                        """, (rs, rowNum) -> map(rs), tenantId, idempotencyKey).stream().findFirst();
    }

    @Override
    public Optional<ToolInvocation> findById(String id) {
        return jdbc.query("SELECT * FROM ai_tool_invocation WHERE id = ?",
                (rs, rowNum) -> map(rs), id).stream().findFirst();
    }

    @Override
    public void save(ToolInvocation invocation) {
        var args = JSON.writeValueAsString(invocation.args());
        var result = invocation.result() == null ? null : JSON.writeValueAsString(invocation.result());
        var updated = jdbc.update("""
                UPDATE ai_tool_invocation
                SET args_json = ?, result_json = ?, status = ?, approval_status = ?, latency_ms = ?,
                    trace_id = ?, updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                """, args, result, invocation.status().name(),
                invocation.approvalStatus() == null ? null : invocation.approvalStatus().name(),
                invocation.latencyMs(), invocation.traceId(), invocation.id());
        if (updated == 0) {
            jdbc.update("""
                    INSERT INTO ai_tool_invocation
                        (id, tenant_id, session_id, user_id, tool_name, args_json, result_json, status,
                         approval_status, idempotency_key, latency_ms, trace_id)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """, invocation.id(), invocation.tenantId(), invocation.sessionId(), invocation.userId(),
                    invocation.toolName(), args, result, invocation.status().name(),
                    invocation.approvalStatus() == null ? null : invocation.approvalStatus().name(),
                    invocation.idempotencyKey(), invocation.latencyMs(), invocation.traceId());
        }
    }

    @Override
    public List<ToolInvocation> findPending(String tenantId) {
        return jdbc.query("""
                        SELECT * FROM ai_tool_invocation
                        WHERE tenant_id = ? AND approval_status = 'PENDING'
                        ORDER BY created_at DESC
                        """, (rs, rowNum) -> map(rs), tenantId);
    }

    @SuppressWarnings("unchecked")
    private ToolInvocation map(java.sql.ResultSet rs) throws java.sql.SQLException {
        var argsJson = rs.getString("args_json");
        var resultJson = rs.getString("result_json");
        var args = argsJson == null ? Map.<String, Object>of() : JSON.readValue(argsJson, Map.class);
        var result = resultJson == null ? null : JSON.readValue(resultJson, Object.class);
        var approval = rs.getString("approval_status");
        return new ToolInvocation(
                rs.getString("id"), rs.getString("tenant_id"), rs.getString("session_id"),
                rs.getString("user_id"), rs.getString("tool_name"), args, result,
                ToolStatus.valueOf(rs.getString("status")),
                approval == null ? ApprovalStatus.NOT_REQUIRED : ApprovalStatus.valueOf(approval),
                rs.getString("idempotency_key"), rs.getLong("latency_ms"), rs.getString("trace_id"),
                toInstant(rs.getTimestamp("created_at")), toInstant(rs.getTimestamp("updated_at")));
    }

    private Instant toInstant(Timestamp timestamp) {
        return timestamp == null ? null : timestamp.toInstant();
    }
}
