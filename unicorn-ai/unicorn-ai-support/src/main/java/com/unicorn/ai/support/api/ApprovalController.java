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

import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.tools.model.ToolInvocation;
import com.unicorn.ai.tools.model.ToolResult;
import com.unicorn.ai.tools.policy.ToolInvocationService;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/ai/support/v1/approvals")
@PreAuthorize("hasAnyRole('SUPPORT_MANAGER','TENANT_ADMIN')")
public class ApprovalController {

    private final ToolInvocationService invocations;

    public ApprovalController(ToolInvocationService invocations) {
        this.invocations = invocations;
    }

    @GetMapping
    public List<ToolInvocation> pending(@AuthenticationPrincipal AiPrincipal principal) {
        return invocations.findPending(principal.tenantId());
    }

    @PostMapping("/{id}/approve")
    public ToolResult approve(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String id) {
        return invocations.approve(id, principal.userId(), principal.roles());
    }

    @PostMapping("/{id}/reject")
    public ToolResult reject(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String id,
                             @RequestBody(required = false) RejectRequest request) {
        return invocations.reject(id, principal.userId(), request == null ? null : request.reason(),
                principal.roles());
    }

    public record RejectRequest(String reason) {
    }
}
