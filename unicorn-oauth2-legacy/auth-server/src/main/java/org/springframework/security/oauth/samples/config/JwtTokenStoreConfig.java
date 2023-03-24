/*
 *    Copyright (c) 2023.  lWoHvYe(Hongyan Wang)
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

package org.springframework.security.oauth.samples.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.jwt.JwtHelper;
import org.springframework.security.jwt.crypto.sign.RsaSigner;
import org.springframework.security.jwt.crypto.sign.RsaVerifier;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.common.util.JsonParser;
import org.springframework.security.oauth2.common.util.JsonParserFactory;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.security.oauth2.provider.approval.ApprovalStore;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.security.oauth2.provider.token.store.JwtAccessTokenConverter;
import org.springframework.security.oauth2.provider.token.store.JwtTokenStore;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class JwtTokenStoreConfig {

    @Autowired
    private ApprovalStore approvalStore;

    //注册TokenStore
    @Bean
    public TokenStore tokenStore() {
        var tokenStore = new JwtTokenStore(accessTokenConverter());
        tokenStore.setApprovalStore(approvalStore);
        return tokenStore;
    }

    //注册JwtAccessTokenConverter
    @Bean
    public JwtAccessTokenConverter accessTokenConverter() {
        final var signer = new RsaSigner(KeyConfig.getSignerKey());

        var converter = new JwtAccessTokenConverter() {
            private JsonParser objectMapper = JsonParserFactory.create();

            @Override
            protected String encode(OAuth2AccessToken accessToken, OAuth2Authentication authentication) {
                String content;
                try {
                    content = this.objectMapper.formatMap(getAccessTokenConverter().convertAccessToken(accessToken, authentication));
                } catch (Exception ex) {
                    throw new IllegalStateException("Cannot convert access token to JSON", ex);
                }
                Map<String, String> headers = new HashMap<>();
                headers.put("kid", KeyConfig.VERIFIER_KEY_ID);
                var token = JwtHelper.encode(content, signer, headers).getEncoded();
                return token;
            }
        };
        converter.setSigner(signer);
        converter.setVerifier(new RsaVerifier(KeyConfig.getVerifierKey()));
        return converter;
    }
}
