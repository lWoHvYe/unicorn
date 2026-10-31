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

package com.unicorn.ai.support.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * PostgreSQL superusers and table owners bypass RLS. Warn or fail fast so a
 * production deployment cannot believe it is isolated while RLS is ineffective.
 */
@Slf4j
@Component
@ConditionalOnProperty(name = "ai.support.rls.enabled", havingValue = "true")
public class RlsRoleVerifier implements ApplicationRunner {

    private final JdbcTemplate jdbc;
    private final boolean failFast;

    public RlsRoleVerifier(JdbcTemplate jdbc,
                           @Value("${ai.support.rls.fail-fast:false}") boolean failFast) {
        this.jdbc = jdbc;
        this.failFast = failFast;
    }

    @Override
    public void run(ApplicationArguments args) {
        var currentUser = jdbc.queryForObject("SELECT current_user", String.class);
        var superuser = jdbc.queryForObject(
                "SELECT usesuper FROM pg_user WHERE usename = current_user", Boolean.class);
        if (Boolean.TRUE.equals(superuser)) {
            var message = "RLS is enabled but database user '" + currentUser
                    + "' is a PostgreSQL superuser; RLS will be bypassed";
            if (failFast) {
                throw new IllegalStateException(message);
            }
            log.error(message);
        }
    }
}
