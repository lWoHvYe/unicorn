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

import com.unicorn.ai.support.async.OutboxRepository;
import com.unicorn.ai.support.security.AiPrincipal;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/ai/support/v1/admin/outbox")
@PreAuthorize("hasRole('TENANT_ADMIN')")
public class OutboxAdminController {

    private final OutboxRepository repository;

    public OutboxAdminController(OutboxRepository repository) {
        this.repository = repository;
    }

    @GetMapping
    public List<OutboxRepository.OutboxEvent> list(@AuthenticationPrincipal AiPrincipal principal,
                                                   @RequestParam(required = false) String status,
                                                   @RequestParam(defaultValue = "50") int limit) {
        return repository.list(principal.tenantId(), status, limit);
    }

    @PostMapping("/{id}/replay")
    public void replay(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String id) {
        repository.replay(principal.tenantId(), id);
    }

    @PostMapping("/{id}/discard")
    public void discard(@AuthenticationPrincipal AiPrincipal principal, @PathVariable String id) {
        repository.discard(principal.tenantId(), id);
    }
}
