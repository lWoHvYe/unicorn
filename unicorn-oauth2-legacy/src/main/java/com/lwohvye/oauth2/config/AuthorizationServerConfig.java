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

package com.lwohvye.oauth2.config;


import com.lwohvye.oauth2.service.UserDetailsServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.config.annotation.configurers.ClientDetailsServiceConfigurer;
import org.springframework.security.oauth2.config.annotation.web.configuration.AuthorizationServerConfigurerAdapter;
import org.springframework.security.oauth2.config.annotation.web.configuration.EnableAuthorizationServer;
import org.springframework.security.oauth2.config.annotation.web.configurers.AuthorizationServerEndpointsConfigurer;
import org.springframework.security.oauth2.provider.token.TokenEnhancer;
import org.springframework.security.oauth2.provider.token.TokenEnhancerChain;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.security.oauth2.provider.token.store.JwtAccessTokenConverter;

import java.util.ArrayList;
import java.util.List;

/**
 * 授权服务器
 */
@Configuration
@EnableAuthorizationServer
public class AuthorizationServerConfig extends AuthorizationServerConfigurerAdapter {

    @Autowired
    private PasswordEncoder passwordEncoder;
    /**
     * 密码模式
     */
    @Autowired
    private AuthenticationManager authenticationManager;
    @Autowired
    private UserDetailsServiceImpl userDetailsServiceImpl;

    /**
     * Jwt配置类
     */
    @Autowired
    JwtAccessTokenConverter jwtAccessTokenConverter;

    @Autowired
    TokenStore tokenStore;

    @Autowired
    JwtTokenEnhancer jwtTokenEnhancer;

    @Override
    public void configure(ClientDetailsServiceConfigurer clients) throws Exception {
        clients.inMemory()//内存中
                .withClient("client_id")//客户端ID
                .secret(passwordEncoder.encode("lwohvye.com"))//秘钥
                .accessTokenValiditySeconds(60) // jwt令牌有效期限
                .refreshTokenValiditySeconds(3600) // refreshToke 有效期限
                .redirectUris("http://localhost:8080", "https://www.bilibili.com")//重定向到的地址
                .scopes("all", "rw", "ro")//授权范围
                .authorizedGrantTypes("authorization_code", "password", "refresh_token")
//                .and()
//                .withClient() // 这里可以配置多个
        ;
    }

    //密码模式需要配置
    @Override
    public void configure(AuthorizationServerEndpointsConfigurer endpoints) {
        // 创建TokenEnhancerChain实例
        var tokenEnhancerChain = new TokenEnhancerChain();
        var enhancerList = new ArrayList<TokenEnhancer>();
        // 配置jtv内容增强器
        enhancerList.add(jwtTokenEnhancer);
        enhancerList.add(jwtAccessTokenConverter);
        tokenEnhancerChain.setTokenEnhancers(enhancerList);

        endpoints.authenticationManager(authenticationManager)
                .userDetailsService(userDetailsServiceImpl)
                // jwt模式
                .tokenStore(tokenStore)
                .accessTokenConverter(jwtAccessTokenConverter)
                .tokenEnhancer(tokenEnhancerChain);
    }
}
