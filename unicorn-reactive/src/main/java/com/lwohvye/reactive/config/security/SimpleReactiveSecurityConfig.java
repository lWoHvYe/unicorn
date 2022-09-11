/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.reactive.config.security;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.SecurityWebFiltersOrder;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.core.userdetails.ReactiveUserDetailsService;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.server.SecurityWebFilterChain;

/**
 * Spring Security依赖上浮比较困难，保留一种对所有请求放行的方式，用于不需要权限认证的环境（比如前面已经有网关做这些了）
 *
 * @date 2022/3/19 5:48 PM
 */
@Slf4j
@EnableGlobalMethodSecurity(prePostEnabled = true, jsr250Enabled = true, securedEnabled = true)
@EnableWebFluxSecurity
@RequiredArgsConstructor
@ConditionalOnExpression("${local.sys.sim-auth:false}") // 基于配置，是否对所有请求放行。默认关闭
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.REACTIVE) // 指定Init Bean的Condition，需要是Reactive（比如WebFlux）
public class SimpleReactiveSecurityConfig {

    @Lazy
    @Autowired
    private ReactiveUserDetailsService userDetailsService;

    @Bean
    public PasswordEncoder passwordEncoder() {
        // 密码加密方式
        return PasswordEncoderFactories.createDelegatingPasswordEncoder();
    }

    @Bean
    SecurityWebFilterChain filterChainSimple(ServerHttpSecurity httpSecurity) {
        return httpSecurity
                .csrf().disable()
                .authorizeExchange(exchanges -> exchanges.anyExchange().permitAll())
                .addFilterBefore(securityAuthFilter(), SecurityWebFiltersOrder.FORM_LOGIN)
                .build();
    }

    private SimpleAuthFilter securityAuthFilter() {
        return new SimpleAuthFilter(userDetailsService);
    }
}
