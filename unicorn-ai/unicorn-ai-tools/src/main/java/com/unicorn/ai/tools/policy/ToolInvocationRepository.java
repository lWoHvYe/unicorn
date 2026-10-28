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

import com.unicorn.ai.tools.model.ToolInvocation;

import java.util.List;
import java.util.Optional;

public interface ToolInvocationRepository {

    Optional<ToolInvocation> findByIdempotencyKey(String tenantId, String idempotencyKey);

    Optional<ToolInvocation> findById(String id);

    Optional<ToolInvocation> findById(String tenantId, String id);

    void save(ToolInvocation invocation);

    List<ToolInvocation> findPending(String tenantId);

    default void saveApproval(String invocationId, String tenantId, String status, String decidedBy,
                              String reason) {
    }
}
