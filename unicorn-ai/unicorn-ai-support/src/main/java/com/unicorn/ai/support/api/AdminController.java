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

import com.unicorn.ai.support.admin.AdminDtos;
import com.unicorn.ai.support.admin.JdbcAdminRepository;
import com.unicorn.ai.support.audit.AuditService;
import com.unicorn.ai.support.security.AiPrincipal;
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
@RequestMapping("/api/ai/support/v1/admin")
@PreAuthorize("hasRole('TENANT_ADMIN')")
public class AdminController {

    private final JdbcAdminRepository repository;
    private final AuditService audit;

    public AdminController(JdbcAdminRepository repository, AuditService audit) {
        this.repository = repository;
        this.audit = audit;
    }

    @GetMapping("/users")
    public List<AdminDtos.UserView> users(@AuthenticationPrincipal AiPrincipal principal) {
        return repository.listUsers(principal.tenantId());
    }

    @PostMapping("/users")
    public AdminDtos.UserView createUser(@AuthenticationPrincipal AiPrincipal principal,
                                         @RequestBody AdminDtos.CreateUserRequest request) {
        var user = repository.createUser(principal.tenantId(), request);
        audit.record(principal, "ADMIN_CREATE_USER", "ai_tenant_user", user.id(), null);
        return user;
    }

    @GetMapping("/roles")
    public List<AdminDtos.RoleView> roles(@AuthenticationPrincipal AiPrincipal principal) {
        return repository.listRoles(principal.tenantId());
    }

    @PostMapping("/roles")
    public AdminDtos.RoleView createRole(@AuthenticationPrincipal AiPrincipal principal,
                                         @RequestBody AdminDtos.CreateRoleRequest request) {
        var role = repository.createRole(principal.tenantId(), request);
        audit.record(principal, "ADMIN_CREATE_ROLE", "ai_role", role.id(), null);
        return role;
    }

    @PostMapping("/users/{userId}/roles/{roleId}")
    public AdminDtos.UserView assignRole(@AuthenticationPrincipal AiPrincipal principal,
                                         @PathVariable String userId, @PathVariable String roleId) {
        var user = repository.assignRole(principal.tenantId(), userId, roleId);
        audit.record(principal, "ADMIN_ASSIGN_ROLE", "ai_tenant_user", userId, "{\"roleId\":\"" + roleId + "\"}");
        return user;
    }
}
