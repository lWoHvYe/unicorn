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

package com.unicorn.ai.rag.job;

import org.springframework.jdbc.core.JdbcTemplate;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Optional;

public class JdbcIngestJobRepository implements IngestJobRepository {

    private final JdbcTemplate jdbcTemplate;

    public JdbcIngestJobRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public void createRunning(String id, String source) {
        jdbcTemplate.update("""
                INSERT INTO ai_rag_ingest_job (id, source, status, chunks)
                VALUES (?, ?, 'RUNNING', 0)
                """, id, source);
    }

    @Override
    public void complete(String id, int chunks) {
        jdbcTemplate.update("""
                UPDATE ai_rag_ingest_job
                SET status = 'COMPLETED', chunks = ?, error = NULL
                WHERE id = ?
                """, chunks, id);
    }

    @Override
    public void fail(String id, String error) {
        jdbcTemplate.update("""
                UPDATE ai_rag_ingest_job
                SET status = 'FAILED', error = ?
                WHERE id = ?
                """, error, id);
    }

    @Override
    public Optional<IngestJob> find(String id) {
        var jobs = jdbcTemplate.query("""
                SELECT id, source, status, chunks, error, created_at, updated_at
                FROM ai_rag_ingest_job
                WHERE id = ?
                """, (rs, rowNum) -> new IngestJob(
                rs.getString("id"),
                rs.getString("source"),
                rs.getString("status"),
                rs.getInt("chunks"),
                rs.getString("error"),
                toInstant(rs.getTimestamp("created_at")),
                toInstant(rs.getTimestamp("updated_at"))), id);
        return jobs.stream().findFirst();
    }

    private Instant toInstant(Timestamp timestamp) {
        return timestamp == null ? null : timestamp.toInstant();
    }
}
