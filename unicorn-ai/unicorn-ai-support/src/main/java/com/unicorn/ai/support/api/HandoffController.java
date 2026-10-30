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
import com.unicorn.ai.support.handoff.HandoffDtos;
import com.unicorn.ai.support.handoff.JdbcHandoffQueryRepository;
import com.unicorn.ai.support.security.AiPrincipal;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/ai/support/v1/handoffs")
public class HandoffController {

    private final JdbcHandoffQueryRepository repository;
    private final AuditService audit;

    public HandoffController(JdbcHandoffQueryRepository repository, AuditService audit) {
        this.repository = repository;
        this.audit = audit;
    }

    @GetMapping
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER','SUPPORT_AGENT')")
    public List<HandoffDtos.HandoffView> list(@AuthenticationPrincipal AiPrincipal principal,
                                              @RequestParam(required = false) String status,
                                              @RequestParam(defaultValue = "0") int offset,
                                              @RequestParam(defaultValue = "50") int limit) {
        return repository.list(principal.tenantId(), status, Math.min(Math.max(limit, 1), 200), Math.max(offset, 0));
    }

    @PostMapping("/{id}/assign")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER')")
    public HandoffDtos.HandoffView assign(@AuthenticationPrincipal AiPrincipal principal,
                                          @PathVariable String id,
                                          @RequestBody HandoffDtos.AssignRequest request) {
        if (request == null || request.assigneeId() == null || request.assigneeId().isBlank()) {
            throw new IllegalArgumentException("assigneeId 不能为空");
        }
        var ticket = repository.assign(principal.tenantId(), id, request.assigneeId());
        audit.record(principal, "HANDOFF_ASSIGN", "ai_handoff_ticket", id, null);
        return ticket;
    }

    @PostMapping("/{id}/close")
    @PreAuthorize("hasAnyRole('TENANT_ADMIN','SUPPORT_MANAGER','SUPPORT_AGENT')")
    public HandoffDtos.HandoffView close(@AuthenticationPrincipal AiPrincipal principal,
                                         @PathVariable String id) {
        var ticket = repository.close(principal.tenantId(), id);
        audit.record(principal, "HANDOFF_CLOSE", "ai_handoff_ticket", id, null);
        return ticket;
    }
}
