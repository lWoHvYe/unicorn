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
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.JwtDecoders;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;

import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;

@Configuration
@ConditionalOnProperty(name = "ai.security.mode", havingValue = "jwt")
public class JwtDecoderConfig {

    @Bean
    JwtDecoder jwtDecoder(SecurityProperties properties) {
        var jwt = properties.jwt();
        if (!jwt.hmacSecret().isBlank()) {
            return NimbusJwtDecoder.withSecretKey(
                    new SecretKeySpec(jwt.hmacSecret().getBytes(StandardCharsets.UTF_8), "HmacSHA256")).build();
        }
        if (!jwt.jwkSetUri().isBlank()) {
            return NimbusJwtDecoder.withJwkSetUri(jwt.jwkSetUri()).build();
        }
        if (!jwt.issuerUri().isBlank()) {
            return JwtDecoders.fromIssuerLocation(jwt.issuerUri());
        }
        throw new IllegalStateException(
                "ai.security.mode=jwt requires ai.security.jwt.issuer-uri, jwk-set-uri or hmac-secret");
    }
}
