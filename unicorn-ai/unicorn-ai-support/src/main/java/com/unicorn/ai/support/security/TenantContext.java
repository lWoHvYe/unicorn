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

package com.unicorn.ai.support.security;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

public final class TenantContext {

    private static final ThreadLocal<AiPrincipal> HOLDER = new ThreadLocal<>();

    private TenantContext() {
    }

    public static void set(AiPrincipal principal) {
        HOLDER.set(principal);
    }

    public static AiPrincipal current() {
        var principal = HOLDER.get();
        if (principal != null) {
            return principal;
        }
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof AiPrincipal value) {
            return value;
        }
        throw new IllegalStateException("缺少租户认证上下文");
    }

    public static void clear() {
        HOLDER.remove();
    }
}
