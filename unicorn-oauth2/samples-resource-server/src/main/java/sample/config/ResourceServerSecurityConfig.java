/*
 * Copyright 2020-2022 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sample.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.web.server.SecurityWebFilterChain;

import static org.springframework.security.config.Customizer.withDefaults;

/**
 * @author Joe Grandja
 * @since 0.0.1
 */
@EnableWebFluxSecurity
@Configuration(proxyBeanMethods = false)
public class ResourceServerSecurityConfig {

    @Bean
    SecurityWebFilterChain springSecurityFilterChain(ServerHttpSecurity http) {
        http
                .csrf(ServerHttpSecurity.CsrfSpec::disable)
                .authorizeExchange(authorizeHttpRequests -> authorizeHttpRequests
                        // 需要特定authority
                        .pathMatchers("/messages").hasAuthority("SCOPE_message.read")
                        .pathMatchers(HttpMethod.GET, "/resource").hasAuthority("SCOPE_resource.read")
                        .pathMatchers(HttpMethod.POST, "/resource").hasAuthority("SCOPE_resource.write")
                        .pathMatchers(HttpMethod.PUT, "/resource").hasAuthority("SCOPE_resource.write")
                        .pathMatchers(HttpMethod.DELETE, "/resource").hasAuthority("SCOPE_resource.write")
                        // 可匿名访问，业务中会有业务逻辑相关的control
                        .pathMatchers("/explore/**").permitAll()
                        // 需要登陆才可访问，这个一般是主体
                        .anyExchange().authenticated())
                .oauth2ResourceServer(oauth2ResourceServer -> oauth2ResourceServer
                        .jwt(withDefaults()));
        return http.build();
    }
}
