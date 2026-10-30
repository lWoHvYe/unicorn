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

package com.unicorn.ai.support.admin;

import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.UUID;

@Repository
public class JdbcAdminRepository {

    private final JdbcTemplate jdbc;

    public JdbcAdminRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public List<AdminDtos.UserView> listUsers(String tenantId) {
        return jdbc.query("""
                SELECT id, external_subject, display_name, email, status
                FROM ai_tenant_user WHERE tenant_id = ? ORDER BY created_at
                """, (rs, rowNum) -> new AdminDtos.UserView(rs.getString("id"),
                rs.getString("external_subject"), rs.getString("display_name"), rs.getString("email"),
                rs.getString("status"), roles(tenantId, rs.getString("id"))), tenantId);
    }

    public AdminDtos.UserView createUser(String tenantId, AdminDtos.CreateUserRequest request) {
        if (request == null || request.externalSubject() == null || request.externalSubject().isBlank()) {
            throw new IllegalArgumentException("externalSubject 不能为空");
        }
        var id = UUID.randomUUID().toString();
        try {
            jdbc.update("""
                    INSERT INTO ai_tenant_user (id, tenant_id, external_subject, display_name, email, status)
                    VALUES (?, ?, ?, ?, ?, 'ACTIVE')
                    """, id, tenantId, request.externalSubject(), request.displayName(), request.email());
        } catch (DuplicateKeyException exception) {
            throw new IllegalArgumentException("该租户下 externalSubject 已存在");
        }
        return new AdminDtos.UserView(id, request.externalSubject(), request.displayName(), request.email(),
                "ACTIVE", List.of());
    }

    public List<AdminDtos.RoleView> listRoles(String tenantId) {
        return jdbc.query("""
                SELECT id, name, scopes FROM ai_role WHERE tenant_id = ? ORDER BY name
                """, (rs, rowNum) -> new AdminDtos.RoleView(rs.getString("id"), rs.getString("name"),
                rs.getString("scopes")), tenantId);
    }

    public AdminDtos.RoleView createRole(String tenantId, AdminDtos.CreateRoleRequest request) {
        if (request == null || request.name() == null || request.name().isBlank()) {
            throw new IllegalArgumentException("name 不能为空");
        }
        var id = UUID.randomUUID().toString();
        try {
            jdbc.update("""
                    INSERT INTO ai_role (id, tenant_id, name, scopes) VALUES (?, ?, ?, ?)
                    """, id, tenantId, request.name(), request.scopes() == null ? "" : request.scopes());
        } catch (DuplicateKeyException exception) {
            throw new IllegalArgumentException("该租户下角色名已存在");
        }
        return new AdminDtos.RoleView(id, request.name(), request.scopes() == null ? "" : request.scopes());
    }

    public AdminDtos.UserView assignRole(String tenantId, String userId, String roleId) {
        var userExists = jdbc.queryForObject("""
                SELECT COUNT(*) FROM ai_tenant_user WHERE tenant_id = ? AND id = ?
                """, Integer.class, tenantId, userId);
        var roleExists = jdbc.queryForObject("""
                SELECT COUNT(*) FROM ai_role WHERE tenant_id = ? AND id = ?
                """, Integer.class, tenantId, roleId);
        if (userExists == null || userExists == 0 || roleExists == null || roleExists == 0) {
            throw new NoSuchElementException("用户或角色不存在");
        }
        try {
            jdbc.update("INSERT INTO ai_user_role (user_id, role_id) VALUES (?, ?)", userId, roleId);
        } catch (DuplicateKeyException ignored) {
            // Idempotent assignment.
        }
        return listUsers(tenantId).stream().filter(user -> user.id().equals(userId)).findFirst()
                .orElseThrow(() -> new NoSuchElementException("用户不存在"));
    }

    private List<String> roles(String tenantId, String userId) {
        return new ArrayList<>(jdbc.queryForList("""
                SELECT r.name FROM ai_role r
                JOIN ai_user_role ur ON ur.role_id = r.id
                WHERE r.tenant_id = ? AND ur.user_id = ? ORDER BY r.name
                """, String.class, tenantId, userId));
    }
}
