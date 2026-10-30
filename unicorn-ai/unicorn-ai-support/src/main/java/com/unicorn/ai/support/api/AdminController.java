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

    public AdminController(JdbcAdminRepository repository) {
        this.repository = repository;
    }

    @GetMapping("/users")
    public List<AdminDtos.UserView> users(@AuthenticationPrincipal AiPrincipal principal) {
        return repository.listUsers(principal.tenantId());
    }

    @PostMapping("/users")
    public AdminDtos.UserView createUser(@AuthenticationPrincipal AiPrincipal principal,
                                         @RequestBody AdminDtos.CreateUserRequest request) {
        return repository.createUser(principal.tenantId(), request);
    }

    @GetMapping("/roles")
    public List<AdminDtos.RoleView> roles(@AuthenticationPrincipal AiPrincipal principal) {
        return repository.listRoles(principal.tenantId());
    }

    @PostMapping("/roles")
    public AdminDtos.RoleView createRole(@AuthenticationPrincipal AiPrincipal principal,
                                         @RequestBody AdminDtos.CreateRoleRequest request) {
        return repository.createRole(principal.tenantId(), request);
    }

    @PostMapping("/users/{userId}/roles/{roleId}")
    public AdminDtos.UserView assignRole(@AuthenticationPrincipal AiPrincipal principal,
                                         @PathVariable String userId, @PathVariable String roleId) {
        return repository.assignRole(principal.tenantId(), userId, roleId);
    }
}
