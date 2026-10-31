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

package com.unicorn.ai.support.quota;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class QuotaRepository {

    private final JdbcTemplate jdbc;

    public QuotaRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public void set(String tenantId, String metric, String period, long limit) {
        var updated = jdbc.update("""
                UPDATE ai_quota SET quota_limit = ?, updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND metric = ? AND period = ?
                """, limit, tenantId, metric, period);
        if (updated == 0) {
            jdbc.update("""
                    INSERT INTO ai_quota (tenant_id, metric, period, quota_limit, used, period_start)
                    VALUES (?, ?, ?, ?, 0, CURRENT_DATE)
                    """, tenantId, metric, period, limit);
        }
    }

    public boolean consume(String tenantId, String metric, String period) {
        var updated = jdbc.update("""
                UPDATE ai_quota
                SET used = CASE
                        WHEN period = 'DAY' AND (period_start IS NULL OR period_start <> CURRENT_DATE)
                            THEN 1
                        ELSE used + 1
                    END,
                    period_start = CASE WHEN period = 'DAY' THEN CURRENT_DATE ELSE period_start END,
                    updated_at = CURRENT_TIMESTAMP
                WHERE tenant_id = ? AND metric = ? AND period = ?
                  AND (period = 'DAY' AND (period_start IS NULL OR period_start <> CURRENT_DATE)
                       OR used < quota_limit)
                """, tenantId, metric, period);
        if (updated > 0) {
            return true;
        }
        var count = jdbc.queryForObject("""
                SELECT COUNT(*) FROM ai_quota WHERE tenant_id = ? AND metric = ? AND period = ?
                """, Integer.class, tenantId, metric, period);
        return count == null || count == 0;
    }
}
