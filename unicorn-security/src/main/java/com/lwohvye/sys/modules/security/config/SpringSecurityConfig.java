/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.sys.modules.security.config;

import com.lwohvye.core.config.security.SimpleSecurityConfig;
import com.lwohvye.core.utils.StringUtils;
import com.lwohvye.core.utils.json.JsonUtils;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.core.utils.result.ResultUtils;
import com.lwohvye.sys.modules.rabbitmq.config.RabbitMQConfig;
import com.lwohvye.sys.modules.rabbitmq.service.RabbitMQProducerService;
import com.lwohvye.sys.modules.security.config.bean.SecurityProperties;
import com.lwohvye.sys.modules.security.security.CustomAuthorizationManager;
import com.lwohvye.sys.modules.security.security.JwtAuthTokenConfigurer;
import com.lwohvye.sys.modules.security.security.TokenProvider;
import com.lwohvye.sys.modules.security.security.filter.CustomInvocationSecurityMetadataSource;
import com.lwohvye.sys.modules.security.security.handler.*;
import com.lwohvye.sys.modules.security.service.dto.JwtUserDto;
import com.lwohvye.sys.modules.system.service.IResourceService;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.access.SecurityMetadataSource;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authorization.AuthorizationManager;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.access.intercept.RequestAuthorizationContext;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.filter.CorsFilter;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * SpringSecurity配置类。若需要不同接口不同的安全策略，可参考{@link CustomSpringBootWebSecurityConfiguration}中的配置方式
 *
 * @author Zheng Jie,Hongyan Wang
 */
// TODO: 2022/11/12 Update to Security 6.0 -> https://docs.spring.io/spring-security/reference/5.8/migration.html
// 添加该注解到@Configuration的类上，应用程序便可以使用自定义的WebSecurityConfigurer或拓展自WebSecurityConfigurerAdapter的配置类来装配Spring Security框架。
// 在5.4开始引入新的配置方式 https://spring.io/blog/2022/02/21/spring-security-without-the-websecurityconfigureradapter
// Spring Security lambda DSL
@EnableWebSecurity
@RequiredArgsConstructor
// 使用 @EnableGlobalMethodSecurity 注解来启用全局方法安全注解功能。该注解提供了三种不同的机制来实现同一种功能，3.0开始使用@EnbaleMethodSecurity替换
// 包括prePostEnabled 、securedEnabled 和 jsr250Enabled 三种方式
// 设置 prePostEnabled 为 true ，则开启了基于表达式的方法安全控制。通过表达式运算结果的布尔值来决定是否可以访问（true 开放， false 拒绝 ）
// 设置 securedEnabled 为 true ，就开启了角色注解 @Secured ，该注解功能要简单的多，默认情况下只能基于角色（默认需要带前缀 ROLE_）集合来进行访问控制决策。
@EnableMethodSecurity(securedEnabled = true)
@Configuration // spring boot 3.0开始要加上这个
@ConditionalOnExpression("!${local.sys.multi-security:false}") // 这里用了取反。非multi时开启。默认开启
@ConditionalOnMissingBean(SimpleSecurityConfig.class) // 如果使用了简单配置，就不加载本配置了
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
public class SpringSecurityConfig {

    private final SecurityProperties properties;
    private final TokenProvider tokenProvider;
    private final CorsFilter corsFilter;
    private final JwtAuthenticationEntryPoint authenticationErrorHandler;
    private final JwtAccessDeniedHandler jwtAccessDeniedHandler;
    private final ApplicationContext applicationContext;
    private final UserDetailsService userDetailsService;
    private final IResourceService resourceService;
    private final RabbitMQProducerService rabbitMQProducerService;

    @Bean
    public PasswordEncoder passwordEncoder() {
        // 密码加密方式
        return new BCryptPasswordEncoder();
    }

    @Bean
    SecurityFilterChain filterChainDefault(HttpSecurity httpSecurity,
                                           AuthenticationSuccessHandler successHandler,
                                           AuthenticationFailureHandler failureHandler) throws Exception {
        return httpSecurity
                // 禁用 CSRF
                // CSRF（跨站点请求伪造：Cross-Site Request Forgery）的。
                // 一般来讲，为了防御CSRF攻击主要有三种策略：验证 HTTP Referer 字段；在请求地址中添加 token 并验证；在 HTTP 头中自定义属性并验证。
                .csrf().disable()
                // 这样注册自定义的UsernamePasswordAuthenticationFilter
                .apply(MyCustomDsl.customDsl(successHandler, failureHandler))
                .and()
                .addFilterBefore(corsFilter, UsernamePasswordAuthenticationFilter.class)
                // 授权异常
                .exceptionHandling()
                .authenticationEntryPoint(authenticationErrorHandler)
                .accessDeniedHandler(jwtAccessDeniedHandler)
                // 定义退出逻辑处理
                .and()
                .logout()
                .logoutUrl("/auth/logout")
                .addLogoutHandler(new CustomLogoutHandler(tokenProvider))
                .logoutSuccessHandler(new CustomLogoutSuccessHandler())
                // 防止iframe 造成跨域
                .and()
                .headers()
                .frameOptions()
                .disable()
                // 不创建会话
                .and()
                .sessionManagement()
                // SessionManagementConfigurer
                // session的创建策略，总是创建【ALWAYS】、需要时创建【IF_REQUIRED】、永不创建【STATELESS】 、永不创建但如有则使用【NEVER】
                .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                .and()
                .authorizeHttpRequests(authorize ->
                        // 所有请求都需要认证
                        authorize.anyRequest().access(customAuthorizationManager()))
                .apply(securityConfigurerAdapter())
                .and().build();
    }

    private JwtAuthTokenConfigurer securityConfigurerAdapter() {
        return new JwtAuthTokenConfigurer(tokenProvider, userDetailsService);
    }

    /**
     * 元数据加载器
     *
     * @return CustomFilterInvocationSecurityMetadataSource
     */
    @Bean
    public SecurityMetadataSource customInvocationSecurityMetadataSource() {
        return new CustomInvocationSecurityMetadataSource(applicationContext, resourceService);
    }

    /**
     * 鉴权
     *
     * @return affirmativeBased
     */
    @Bean
    public AuthorizationManager<RequestAuthorizationContext> customAuthorizationManager() {
        return new CustomAuthorizationManager<>(customInvocationSecurityMetadataSource());
    }

    // endregion

    // region loginFilter、handler

    /**
     * 处理登录成功后返回 JWT Token 对.
     *
     * @return the authentication success handler
     */
    @Bean
    public AuthenticationSuccessHandler authenticationSuccessHandler() {
        return (request, response, authentication) -> {
            if (response.isCommitted()) {
                return;
            }
            SecurityContextHolder.getContext().setAuthentication(authentication);
            // 生成令牌与第三方系统获取令牌方式
            // UserDetails userDetails = userDetailsService.loadUserByUsername(userInfo.getUsername());
            // Authentication authentication = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
            // SecurityContextHolder.getContext().setAuthentication(authentication);
            var token = tokenProvider.createToken(authentication);
            final var jwtUserDto = (JwtUserDto) authentication.getPrincipal();
            // 用户登录成功后，写一条消息
            var authSuccessMsg = new AmqpMsgEntity().setMsgType("authSave").setMsgData(jwtUserDto.getUser().toString()).setExtraData("saveAuthorizeLog");
            rabbitMQProducerService.sendMsg(RabbitMQConfig.DIRECT_SYNC_EXCHANGE, RabbitMQConfig.AUTH_LOCAL_ROUTE_KEY, authSuccessMsg);

            // 返回 token 与 用户信息
            var authInfo = Map.of("id_token", properties.getTokenStartWith() + token, "user", jwtUserDto);
            // 这里需要进行响应
            ResultUtils.resultJson(response, HttpServletResponse.SC_OK, JsonUtils.toJSONString(authInfo));
        };
    }

    /**
     * 失败登录处理器 处理登录失败后的逻辑 登录失败返回信息 以此为依据跳转
     *
     * @return the authentication failure handler
     */
    @Bean
    public AuthenticationFailureHandler authenticationFailureHandler() {
        return (request, response, authenticationException) -> {
            if (response.isCommitted()) {
                return;
            }
            // 针对于密码错误行为，需进行记录
            if (authenticationException instanceof BadCredentialsException) {
                var username = request.getAttribute("username");
                if (Objects.nonNull(username)) {
                    var ip = StringUtils.getIp(request);
                    var lockedIp = ip + "||authLocked||";

                    var infoMap = new HashMap<String, Object>();
                    infoMap.put("ip", ip);
                    infoMap.put("username", username);
                    infoMap.put("lockedIp", lockedIp);
                    var authFailedMsg = new AmqpMsgEntity().setMsgType("auth").setMsgData(JsonUtils.toJSONString(infoMap)).setExtraData("solveAuthFailed");
                    //  发送消息，因为在Consumer侧限制了MsgType，所以Success与Failure虽然配置差不多，但只有正确的Consumer会成功消费，其他的会ignore
                    rabbitMQProducerService.sendMsg(RabbitMQConfig.DIRECT_SYNC_EXCHANGE, RabbitMQConfig.AUTH_LOCAL_ROUTE_KEY, authFailedMsg);
                }
            }
            // 返回错误信息。用下面的sendError会被EntryPoint拦截并覆盖。
//            response.sendError(HttpServletResponse.SC_BAD_REQUEST, authenticationException.getMessage());
            ResultUtils.resultJson(response, HttpServletResponse.SC_BAD_REQUEST, authenticationException.getMessage());
        };
    }

    // endregion

}
