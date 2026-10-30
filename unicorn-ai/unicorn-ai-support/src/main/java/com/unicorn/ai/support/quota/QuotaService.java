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

import org.springframework.stereotype.Service;

@Service
public class QuotaService {

    public static final String CHAT = "chat";
    public static final String DAY = "DAY";

    private final QuotaRepository repository;

    public QuotaService(QuotaRepository repository) {
        this.repository = repository;
    }

    public void consumeChat(String tenantId) {
        if (!repository.consume(tenantId, CHAT, DAY)) {
            throw new QuotaExceededException("租户 " + tenantId + " 的每日聊天配额已用尽");
        }
    }

    public void set(String tenantId, String metric, String period, long limit) {
        if (metric == null || metric.isBlank() || limit <= 0) {
            throw new IllegalArgumentException("metric 不能为空且 limit 必须大于 0");
        }
        repository.set(tenantId, metric, period == null || period.isBlank() ? DAY : period, limit);
    }
}
