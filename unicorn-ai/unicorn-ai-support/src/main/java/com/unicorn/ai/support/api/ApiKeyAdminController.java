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

package com.unicorn.ai.support.api;

import com.unicorn.ai.support.audit.AuditService;
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.security.ApiKeyRepository;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/ai/support/v1/admin/api-keys")
@PreAuthorize("hasRole('TENANT_ADMIN')")
public class ApiKeyAdminController {

    private final ApiKeyRepository repository;
    private final AuditService audit;

    public ApiKeyAdminController(ApiKeyRepository repository, AuditService audit) {
        this.repository = repository;
        this.audit = audit;
    }

    @PostMapping
    public CreateApiKeyResponse create(@AuthenticationPrincipal AiPrincipal principal,
                                       @RequestBody CreateApiKeyRequest request) {
        if (request == null || request.userId() == null || request.userId().isBlank()
                || request.roles() == null || request.roles().isBlank()) {
            throw new IllegalArgumentException("userId 和 roles 不能为空");
        }
        var key = repository.create(principal.tenantId(), request.userId(), request.roles());
        audit.record(principal, "API_KEY_CREATE", "ai_api_key", key.id(), null);
        return new CreateApiKeyResponse(key.id(), key.apiKey());
    }

    @PostMapping("/{id}/revoke")
    public void revoke(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String id) {
        repository.revoke(principal.tenantId(), id);
        audit.record(principal, "API_KEY_REVOKE", "ai_api_key", id, null);
    }

    public record CreateApiKeyRequest(String userId, String roles) {
    }

    public record CreateApiKeyResponse(String id, String apiKey) {
    }
}
