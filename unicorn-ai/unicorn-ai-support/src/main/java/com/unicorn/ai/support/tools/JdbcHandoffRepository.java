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

import com.unicorn.ai.tools.domain.HandoffRepository;
import com.unicorn.ai.tools.model.HandoffTicket;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public class JdbcHandoffRepository implements HandoffRepository {

    private final JdbcTemplate jdbc;

    public JdbcHandoffRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public HandoffTicket create(String tenantId, String sessionId, String requesterId, String reason) {
        var id = UUID.randomUUID().toString();
        jdbc.update("""
                INSERT INTO ai_handoff_ticket
                    (id, tenant_id, session_id, requester_id, reason, status, priority)
                VALUES (?, ?, ?, ?, ?, 'OPEN', 'NORMAL')
                """, id, tenantId, sessionId, requesterId, reason);
        return new HandoffTicket(id, tenantId, reason, "OPEN");
    }
}
