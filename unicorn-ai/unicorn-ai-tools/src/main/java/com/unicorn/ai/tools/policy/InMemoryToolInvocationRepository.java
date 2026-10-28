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

package com.unicorn.ai.tools.policy;

import com.unicorn.ai.tools.model.ApprovalStatus;
import com.unicorn.ai.tools.model.ToolInvocation;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class InMemoryToolInvocationRepository implements ToolInvocationRepository {

    private final ConcurrentMap<String, ToolInvocation> invocations = new ConcurrentHashMap<>();

    @Override
    public Optional<ToolInvocation> findByIdempotencyKey(String tenantId, String idempotencyKey) {
        return invocations.values().stream()
                .filter(invocation -> invocation.tenantId().equals(tenantId))
                .filter(invocation -> invocation.idempotencyKey().equals(idempotencyKey))
                .findFirst();
    }

    @Override
    public Optional<ToolInvocation> findById(String id) {
        return Optional.ofNullable(invocations.get(id));
    }

    @Override
    public void save(ToolInvocation invocation) {
        invocations.put(invocation.id(), invocation);
    }

    @Override
    public List<ToolInvocation> findPending(String tenantId) {
        return invocations.values().stream()
                .filter(invocation -> invocation.tenantId().equals(tenantId))
                .filter(invocation -> invocation.approvalStatus() == ApprovalStatus.PENDING)
                .toList();
    }

    public Map<String, ToolInvocation> all() {
        return Map.copyOf(invocations);
    }
}
