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

import com.unicorn.ai.support.config.SecurityProperties;
import org.junit.jupiter.api.Test;
import org.springframework.security.oauth2.jwt.Jwt;

import java.time.Instant;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class JwtSecurityConfigTest {

    private static final String SECRET = "0123456789abcdef0123456789abcdef";

    @Test
    void hmacDecoderIsRunnable() {
        var properties = new SecurityProperties("jwt", "X-API-Key",
                new SecurityProperties.Jwt(true, "", "", SECRET));
        assertNotNull(new JwtDecoderConfig().jwtDecoder(properties));
    }

    @Test
    void missingDecoderConfigurationFailsFast() {
        var properties = new SecurityProperties("jwt", "X-API-Key",
                new SecurityProperties.Jwt(true, "", "", ""));
        assertThrows(IllegalStateException.class, () -> new JwtDecoderConfig().jwtDecoder(properties));
    }

    @Test
    void mapsTenantAndRolesFromClaims() {
        var jwt = new Jwt("token", Instant.now(), Instant.now().plusSeconds(60), Map.of("alg", "HS256"),
                Map.of("tenant_id", "demo", "sub", "u1", "roles", List.of("SUPPORT_AGENT")));
        var authentication = new JwtSecurityConfig().jwtAuthenticationConverter().convert(jwt);
        assertNotNull(authentication);
        var principal = (AiPrincipal) authentication.getPrincipal();
        assertEquals("demo", principal.tenantId());
        assertEquals("u1", principal.userId());
        assertEquals(List.of("ROLE_SUPPORT_AGENT"), authentication.getAuthorities().stream()
                .map(Object::toString).toList());
    }
}
