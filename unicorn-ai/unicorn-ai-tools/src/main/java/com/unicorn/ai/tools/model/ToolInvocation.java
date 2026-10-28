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

package com.unicorn.ai.tools.model;

import java.time.Instant;
import java.util.Map;

public record ToolInvocation(String id, String tenantId, String sessionId, String userId, String toolName,
                             Map<String, Object> args, Object result, ToolStatus status,
                             ApprovalStatus approvalStatus, String idempotencyKey, long latencyMs,
                             String traceId, Instant createdAt, Instant updatedAt) {

    public ToolInvocation withResult(ToolStatus newStatus, ApprovalStatus newApprovalStatus,
                                     Object newResult, long newLatencyMs) {
        return new ToolInvocation(id, tenantId, sessionId, userId, toolName, args, newResult,
                newStatus, newApprovalStatus, idempotencyKey, newLatencyMs, traceId, createdAt, Instant.now());
    }

    public ToolInvocation withApproval(ApprovalStatus newApprovalStatus) {
        return new ToolInvocation(id, tenantId, sessionId, userId, toolName, args, result,
                status, newApprovalStatus, idempotencyKey, latencyMs, traceId, createdAt, Instant.now());
    }
}
