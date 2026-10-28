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
import org.springframework.security.oauth2.core.DelegatingOAuth2TokenValidator;
import org.springframework.security.oauth2.core.OAuth2TokenValidator;
import org.springframework.security.oauth2.jwt.JwtClaimValidator;
import org.springframework.security.oauth2.jwt.JwtValidators;

import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

@Configuration
@ConditionalOnProperty(name = "ai.security.mode", havingValue = "jwt")
public class JwtDecoderConfig {

    @Bean
    JwtDecoder jwtDecoder(SecurityProperties properties) {
        var jwt = properties.jwt();
        JwtDecoder decoder;
        if (!jwt.hmacSecret().isBlank()) {
            decoder = NimbusJwtDecoder.withSecretKey(
                    new SecretKeySpec(jwt.hmacSecret().getBytes(StandardCharsets.UTF_8), "HmacSHA256")).build();
        } else if (!jwt.jwkSetUri().isBlank()) {
            decoder = NimbusJwtDecoder.withJwkSetUri(jwt.jwkSetUri()).build();
        } else if (!jwt.issuerUri().isBlank()) {
            decoder = JwtDecoders.fromIssuerLocation(jwt.issuerUri());
        } else {
            throw new IllegalStateException(
                    "ai.security.mode=jwt requires ai.security.jwt.issuer-uri, jwk-set-uri or hmac-secret");
        }
        return withValidators(decoder, jwt);
    }

    private JwtDecoder withValidators(JwtDecoder decoder, SecurityProperties.Jwt jwt) {
        var validators = new ArrayList<OAuth2TokenValidator<org.springframework.security.oauth2.jwt.Jwt>>();
        if (!jwt.issuerUri().isBlank()) {
            validators.add(JwtValidators.createDefaultWithIssuer(jwt.issuerUri()));
        } else {
            validators.add(JwtValidators.createDefault());
        }
        if (!jwt.audience().isBlank()) {
            validators.add(new JwtClaimValidator<java.util.List<String>>("aud",
                    audience -> audience != null && audience.contains(jwt.audience())));
        }
        var validator = new DelegatingOAuth2TokenValidator<>(validators);
        return token -> {
            var decoded = decoder.decode(token);
            validator.validate(decoded);
            return decoded;
        };
    }
}
