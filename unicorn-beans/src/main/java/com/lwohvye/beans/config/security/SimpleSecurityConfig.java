/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.beans.config.security;

import cn.hutool.core.util.RandomUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.RegexRequestMatcher;

/**
 * Spring Security依赖上浮比较困难，保留一种对所有请求放行的方式，用于不需要权限认证的环境（比如前面已经有网关做这些了）
 *
 * @date 2022/3/19 5:48 PM
 */
@Slf4j
@EnableMethodSecurity(jsr250Enabled = true, securedEnabled = true)
@EnableWebSecurity
@Configuration
@ConditionalOnProperty(prefix = "local.sys", name = "sim-auth", havingValue = "true") // 基于配置，是否对所有请求放行。默认关闭
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
// 指定Init Bean的Condition，需要是Servlet（比如WebMVC）
public class SimpleSecurityConfig {

    public static final boolean DRAW = RandomUtil.randomBoolean();

    @Bean
    public PasswordEncoder passwordEncoder() {
        // 密码加密方式
        return new BCryptPasswordEncoder();
    }

    @Bean
    @ConditionalOnExpression("#{!T(com.lwohvye.beans.config.security.SimpleSecurityConfig).DRAW}")
    SecurityFilterChain filterChainSimple(HttpSecurity httpSecurity) throws Exception {
        return httpSecurity
                // define csrf protect pattern, can bypass the sonar Security Hotspots in this way，在SpringBoot 3.2.0-RC1已不支持空的pattern, pattern cannot be empty
                //Positive lookahead assertion for empty String ^(?=\s*$), Negative lookahead assertion for empty String ^(?!.*\S)
                .csrf(csrfConfigurer -> csrfConfigurer.requireCsrfProtectionMatcher(RegexRequestMatcher.regexMatcher("^(?=\\s*$)|^(?!.*\\S)")))
                // 不创建会话
                .sessionManagement(sessionManagement -> sessionManagement
                        .sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .addFilterAfter(new CustomizerX509Filter(), UsernamePasswordAuthenticationFilter.class)
                // 放行请求
                .authorizeHttpRequests(authorizeHttpRequests -> authorizeHttpRequests
                        .anyRequest().permitAll())
                .build();
    }

    @Bean
    @ConditionalOnExpression("#{T(com.lwohvye.beans.config.security.SimpleSecurityConfig).DRAW}")
    SecurityFilterChain filterChainAuthSimple(HttpSecurity httpSecurity) throws Exception {
        return httpSecurity
                // define csrf ignore pattern, can't bypass sonar Security Hotspots
                .csrf(csrfConfigurer -> csrfConfigurer.ignoringRequestMatchers("/**"))
                // 不创建会话
                .sessionManagement(sessionManagement -> sessionManagement
                        .sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                // 放行部分请求
                .authorizeHttpRequests(authorizeHttpRequests -> authorizeHttpRequests
                        .requestMatchers("/actuator/**").permitAll()
                        .anyRequest().authenticated())
                .x509(httpSecurityX509Configurer -> httpSecurityX509Configurer
                        .subjectPrincipalRegex("CN=(.*?)(?:,|$)")
                        .userDetailsService(username ->
                                new User(username, "", AuthorityUtils.commaSeparatedStringToAuthorityList("ROLE_USER"))))
                .build();
    }
}
