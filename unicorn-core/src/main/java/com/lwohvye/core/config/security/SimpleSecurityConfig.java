/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.config.security;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;

/**
 * Spring Security依赖上浮比较困难，保留一种对所有请求放行的方式，用于不需要权限认证的环境（比如前面已经有网关做这些了）
 *
 * @date 2022/3/19 5:48 PM
 */
@Slf4j
@EnableMethodSecurity(jsr250Enabled = true, securedEnabled = true)
@EnableWebSecurity
@Configuration
@ConditionalOnExpression("${local.sys.sim-auth:false}") // 基于配置，是否对所有请求放行。默认关闭
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET) // 指定Init Bean的Condition，需要是Servlet（比如WebMVC）
public class SimpleSecurityConfig {

    @Lazy
    @Autowired
    private UserDetailsService userDetailsService;

    @Bean
    public PasswordEncoder passwordEncoder() {
        // 密码加密方式
        return new BCryptPasswordEncoder();
    }

    @Bean
    SecurityFilterChain filterChainSimple(HttpSecurity httpSecurity) throws Exception {
        return httpSecurity
                .csrf().disable()
                // 不创建会话
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
                // 放行所有请求
                .authorizeHttpRequests().anyRequest().permitAll().and()
                .apply(securityConfigurerAdapter()).and()
                .build();
    }

    private SimpleAuthConfigurer securityConfigurerAdapter() {
        return new SimpleAuthConfigurer(userDetailsService);
    }
}
