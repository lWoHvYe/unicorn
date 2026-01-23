/*
 *    Copyright (c) 2024-2026.  lWoHvYe(Hongyan Wang)
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

package sample.init;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.ClientAuthenticationMethod;
import org.springframework.security.oauth2.jose.jws.SignatureAlgorithm;
import org.springframework.security.oauth2.server.authorization.client.RegisteredClient;
import org.springframework.security.oauth2.server.authorization.settings.ClientSettings;
import org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat;
import org.springframework.security.oauth2.server.authorization.settings.TokenSettings;
import org.springframework.stereotype.Component;
import sample.service.RedisRegisteredClientRepository;

import java.time.Duration;
import java.time.Instant;
import java.util.List;

@Component
public class AuthServerPostProcessor implements ApplicationListener<ContextRefreshedEvent> {

    @Autowired
    RedisRegisteredClientRepository registeredClientRepository;

    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        if (event.getApplicationContext().getParent() == null) {//root application context 没有parent，再执行这个.
            var loginClient = RegisteredClient.withId("74b93558-77bf-4b6e-8ee6-d4f262fce099")
                    .clientId("login-client").clientIdIssuedAt(Instant.now())
                    .clientSecret("$2a$10$9ld5pThtd0BtivV7aWF1vOeiAB5havfmjz8d/g9MVTHnUVzHYy0Za")
                    .clientName("74b93558-77bf-4b6e-8ee6-d4f262fce099")
                    .clientAuthenticationMethod(ClientAuthenticationMethod.CLIENT_SECRET_BASIC)
                    .authorizationGrantTypes(authorizationGrantTypes ->
                            authorizationGrantTypes.addAll(List.of(AuthorizationGrantType.REFRESH_TOKEN, AuthorizationGrantType.AUTHORIZATION_CODE)))
                    .redirectUri("http://127.0.0.1:8080/login/oauth2/code/login-client")
                    .scopes(scopes -> scopes.addAll(List.of("openid", "resource.read", "profile", "email", "resource.write")))
                    .clientSettings(ClientSettings.builder()
                            .requireProofKey(false)
                            .requireAuthorizationConsent(true)
                            .build())
                    .tokenSettings(TokenSettings.builder()
                            .reuseRefreshTokens(true)
                            .idTokenSignatureAlgorithm(SignatureAlgorithm.RS256)
                            .authorizationCodeTimeToLive(Duration.ofMinutes(5L))
                            .accessTokenTimeToLive(Duration.ofMinutes(5L))
                            .refreshTokenTimeToLive(Duration.ofHours(1L))
                            .accessTokenFormat(OAuth2TokenFormat.SELF_CONTAINED)
                            .build())
                    .build();
            registeredClientRepository.save(loginClient);
            var messagingClient = RegisteredClient.withId("87d1cc25-772b-4de5-b523-eb65a3af0264")
                    .clientId("messaging-client").clientIdIssuedAt(Instant.now())
                    .clientSecret("$2a$10$LS.I40Br17yt0sYZxKiyqe5QPYBUZIvhkKyKnmRTmUT1nHdC07jmG")
                    .clientName("87d1cc25-772b-4de5-b523-eb65a3af0264")
                    .clientAuthenticationMethod(ClientAuthenticationMethod.CLIENT_SECRET_BASIC)
                    .authorizationGrantTypes(authorizationGrantTypes ->
                            authorizationGrantTypes.addAll(List.of(AuthorizationGrantType.REFRESH_TOKEN, AuthorizationGrantType.CLIENT_CREDENTIALS, AuthorizationGrantType.AUTHORIZATION_CODE)))
                    .redirectUris(redirectUris -> redirectUris.addAll(List.of("http://127.0.0.1:8080/authorized", "http://127.0.0.1:8080/login/oauth2/code/messaging-client-oidc")))
                    .scopes(scopes -> scopes.addAll(List.of("openid", "resource.read", "profile", "message.read", "resource.write", "message.write")))
                    .clientSettings(ClientSettings.builder()
                            .requireProofKey(false)
                            .requireAuthorizationConsent(true)
                            .build())
                    .tokenSettings(TokenSettings.builder()
                            .reuseRefreshTokens(true)
                            .idTokenSignatureAlgorithm(SignatureAlgorithm.RS256)
                            .authorizationCodeTimeToLive(Duration.ofMinutes(5L))
                            .accessTokenTimeToLive(Duration.ofMinutes(5L))
                            .refreshTokenTimeToLive(Duration.ofHours(1L))
                            .accessTokenFormat(OAuth2TokenFormat.SELF_CONTAINED)
                            .build())
                    .build();
            registeredClientRepository.save(messagingClient);
        }
    }
}
