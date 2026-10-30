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

import java.util.List;

public final class AdminDtos {

    private AdminDtos() {
    }

    public record UserView(String id, String externalSubject, String displayName, String email,
                           String status, List<String> roles) {
    }

    public record CreateUserRequest(String externalSubject, String displayName, String email) {
    }

    public record RoleView(String id, String name, String scopes) {
    }

    public record CreateRoleRequest(String name, String scopes) {
    }

    public record AssignRoleRequest(String roleId) {
    }

    public record QuotaRequest(String metric, String period, long limit) {
    }
}
