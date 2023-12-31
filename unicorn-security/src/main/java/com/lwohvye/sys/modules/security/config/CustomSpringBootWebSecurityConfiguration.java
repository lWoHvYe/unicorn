/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.sys.modules.security.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;

/**
 * 多个策略配置。欲使用，可在配置文件中开启
 *
 * @author Hongyan Wang
 * @date 2021/11/28 10:24 上午
 * @see org.springframework.boot.autoconfigure.security.servlet.SpringBootWebSecurityConfiguration
 */
@ConditionalOnProperty(prefix = "local.sys", name = "multi-security")
@Slf4j
@EnableMethodSecurity(jsr250Enabled = true, securedEnabled = true)
@EnableWebSecurity
@Configuration
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
// Spring Security 5.4开始，新的定义方式 https://github.com/spring-projects/spring-security/issues/8804
public class CustomSpringBootWebSecurityConfiguration {


    // @Bean
    // SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    //     return http
    //             .antMatcher("/**")
    //             .authorizeRequests(authorize -> authorize
    //                     .anyRequest().authenticated()
    //             )
    //             .build();
    // }

    @Bean
    public PasswordEncoder passwordEncoder() {
        // 密码加密方式
        return new BCryptPasswordEncoder();
    }

    // region 后台接口安全策略

    // 如果 Spring IoC 容器中存在了多个UserDetailsService，那么这些UserDetailsService就不会生效，影响DaoAuthenticationProvider的注入。
    // 解决方式为在configure(HttpSecurity http)中，通过
    // SharedObject是Spring Security提供的一个非常好用的功能，如果需要在不同的地方需要对一个对象重复使用就可以将它注册为SharedObject，甚至直接注入Spring IoC像下面这样获取就可以了。
    // 这个特性能够简化配置，提高代码的可读性，也为Spring Security的DSL特性打下了基础
    //   var context = http.getSharedObject(ApplicationContext.class);
    //   var xxxAuthenticationProvider = context.getBean("xxxAuthenticationProvider", xxxAuthenticationProvider.class);
    //   http.authenticationProvider(xxxAuthenticationProvider) 注入
    @Bean("daoAuthenticationProvider4Admin")
    DaoAuthenticationProvider daoAuthenticationProvider4Admin() {
        var daoAuthenticationProvider = new DaoAuthenticationProvider();
        //用户详情服务个性化
        daoAuthenticationProvider.setUserDetailsService(username -> {
            // 自行实现获取UserDetails逻辑。若在其他处实现，这里注入一下
            log.warn("Admin-Api is in use");
            return null;
        });
        // 也可以设计特定的密码策略
        var bCryptPasswordEncoder = new BCryptPasswordEncoder();
        daoAuthenticationProvider.setPasswordEncoder(bCryptPasswordEncoder);
        return daoAuthenticationProvider;
    }

    // 在HttpSecurityConfiguration中
    // HttpSecurity被@Scope("prototype")标记。也就是这个HttpSecurity Bean不是单例的，每一次请求都会构造一个新的实例。
    // 这个设定非常方便构建多个互相没有太多关联的SecurityFilterChain，进而能在一个安全体系中构建相互隔离的安全策略。
    @Bean
    SecurityFilterChain filterChain4Admin(HttpSecurity http) throws Exception {
        var context = http.getSharedObject(ApplicationContext.class);
        var daoAuthenticationProvider4Admin = context.getBean("daoAuthenticationProvider4Admin", AuthenticationProvider.class);
        // 根据需求自行定制。首个antMatcher指定了该配置生效的范围
        return http.securityMatcher("/admin/v2")
                .authenticationProvider(daoAuthenticationProvider4Admin)
                .sessionManagement(Customizer.withDefaults())
                .formLogin(Customizer.withDefaults())
                .build();
    }

    // endregion

    // region app接口安全策略

    @Bean("daoAuthenticationProvider4App")
    DaoAuthenticationProvider daoAuthenticationProvider4App() {
        var daoAuthenticationProvider = new DaoAuthenticationProvider();
        //用户详情服务个性化
        daoAuthenticationProvider.setUserDetailsService(username -> {
            // 自行实现获取UserDetails逻辑
            log.warn("App-Api is in use");
            return null;
        });
        // 也可以设计特定的密码策略
        var bCryptPasswordEncoder = new BCryptPasswordEncoder();
        daoAuthenticationProvider.setPasswordEncoder(bCryptPasswordEncoder);
        return daoAuthenticationProvider;
    }

    @Bean
    SecurityFilterChain filterChain4App(HttpSecurity http) throws Exception {
        var context = http.getSharedObject(ApplicationContext.class);
        var daoAuthenticationProvider4App = context.getBean("daoAuthenticationProvider4App", AuthenticationProvider.class);
        // 根据需求自行定制。首个antMatcher指定了该配置生效的范围
        return http.securityMatcher("/app/v2")
                .authenticationProvider(daoAuthenticationProvider4App)
                .sessionManagement(Customizer.withDefaults())
                .formLogin(Customizer.withDefaults())
                .build();
    }
    // endregion
}
