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

import java.util.Map;
import java.util.Set;

public record ToolExecutionContext(String tenantId, String userId, Set<String> roles, String sessionId,
                                   String idempotencyKey, String traceId, Map<String, Object> attributes) {

    public ToolExecutionContext(String tenantId, String userId, Set<String> roles, String sessionId,
                                String idempotencyKey, String traceId) {
        this(tenantId, userId, roles, sessionId, idempotencyKey, traceId, Map.of());
    }

    public ToolExecutionContext {
        attributes = attributes == null ? Map.of() : Map.copyOf(attributes);
    }

    public boolean hasRole(String role) {
        return roles != null && roles.contains(role);
    }

    public Object attribute(String key) {
        return attributes.get(key);
    }
}
