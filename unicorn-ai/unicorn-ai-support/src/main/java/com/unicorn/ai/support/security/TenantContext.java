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

import java.util.function.Supplier;

/**
 * Java 25 ScopedValue-based request context. Prefer passing AiPrincipal explicitly
 * through async boundaries; this scope is for the synchronous MVC request thread.
 */
public final class TenantContext {

    public static final ScopedValue<AiPrincipal> PRINCIPAL = ScopedValue.newInstance();

    private TenantContext() {
    }

    public static java.util.Optional<AiPrincipal> optional() {
        if (PRINCIPAL.isBound()) {
            return java.util.Optional.of(PRINCIPAL.get());
        }
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof AiPrincipal value) {
            return java.util.Optional.of(value);
        }
        return java.util.Optional.empty();
    }

    public static AiPrincipal current() {
        return optional().orElseThrow(() -> new IllegalStateException("缺少租户认证上下文"));
    }

    public static <T, X extends Throwable> T call(AiPrincipal principal,
                                                 ScopedValue.CallableOp<T, X> action) throws X {
        return ScopedValue.where(PRINCIPAL, principal).call(action);
    }

    public static void run(AiPrincipal principal, Runnable action) {
        ScopedValue.where(PRINCIPAL, principal).run(action);
    }

    public static <T> T callUnchecked(AiPrincipal principal, Supplier<T> action) {
        try {
            return ScopedValue.where(PRINCIPAL, principal).call(action::get);
        } catch (RuntimeException | Error exception) {
            throw exception;
        } catch (Throwable exception) {
            throw new IllegalStateException("租户上下文执行失败", exception);
        }
    }
}
