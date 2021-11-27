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
package com.lwohvye.modules.security.config;

import com.lwohvye.annotation.AnonymousAccess;
import com.lwohvye.config.rabbitmq.RabbitMqConfig;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import com.lwohvye.modules.rabbitmq.service.RabbitMQProducerService;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import com.lwohvye.modules.security.security.handler.CustomLogoutHandler;
import com.lwohvye.modules.security.security.handler.CustomLogoutSuccessHandler;
import com.lwohvye.modules.security.security.handler.JwtAccessDeniedHandler;
import com.lwohvye.modules.security.security.JwtAuthTokenConfigurer;
import com.lwohvye.modules.security.security.handler.JwtAuthenticationEntryPoint;
import com.lwohvye.modules.security.security.TokenProvider;
import com.lwohvye.modules.security.security.filter.CustomAuthenticationFilter;
import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.utils.JsonUtils;
import com.lwohvye.utils.ResultUtil;
import com.lwohvye.utils.StringUtils;
import com.lwohvye.utils.enums.RequestMethodEnum;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.util.Pair;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.core.GrantedAuthorityDefaults;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.filter.CorsFilter;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.condition.PathPatternsRequestCondition;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import javax.servlet.http.HttpServletResponse;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 */
@Configuration
// 添加该注解到@Configuration的类上，应用程序便可以使用自定义的WebSecurityConfigurer或拓展自WebSecurityConfigurerAdapter的配置类来装配Spring Security框架。
@EnableWebSecurity
@RequiredArgsConstructor
@EnableGlobalMethodSecurity(prePostEnabled = true, securedEnabled = true)
public class SpringSecurityConfig extends WebSecurityConfigurerAdapter {

    private final SecurityProperties properties;
    private final TokenProvider tokenProvider;
    private final CorsFilter corsFilter;
    private final JwtAuthenticationEntryPoint authenticationErrorHandler;
    private final JwtAccessDeniedHandler jwtAccessDeniedHandler;
    private final ApplicationContext applicationContext;
    private final UserDetailsService userDetailsService;
    private final RabbitMQProducerService rabbitMQProducerService;

    @Bean
    GrantedAuthorityDefaults grantedAuthorityDefaults() {
        // 去除 ROLE_ 前缀
        return new GrantedAuthorityDefaults("");
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        // 密码加密方式
        return new BCryptPasswordEncoder();
    }

    @Override
    protected void configure(HttpSecurity httpSecurity) throws Exception {
        // 搜寻匿名标记 url： @AnonymousAccess
        RequestMappingHandlerMapping requestMappingHandlerMapping = (RequestMappingHandlerMapping) applicationContext.getBean("requestMappingHandlerMapping");
        Map<RequestMappingInfo, HandlerMethod> handlerMethodMap = requestMappingHandlerMapping.getHandlerMethods();
        // 获取匿名标记
        Map<String, Set<String>> anonymousUrls = getAnonymousUrl(handlerMethodMap);
        httpSecurity
                // 禁用 CSRF
                // CSRF（跨站点请求伪造：Cross-Site Request Forgery）的。
                // 一般来讲，为了防御CSRF攻击主要有三种策略：验证 HTTP Referer 字段；在请求地址中添加 token 并验证；在 HTTP 头中自定义属性并验证。
                .csrf().disable()
                //用重写的Filter替换掉原有的UsernamePasswordAuthenticationFilter
                // （这里实际上是放到了前面，security自带的Filter在轮到自己执行的时候，会判断当前登录状态，如果已经被之前的Filter验证过了，自己这关就直接放行）
                .addFilterAt(customAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class)
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
                .authorizeRequests()
                // 静态资源等等
                .antMatchers(HttpMethod.GET, "/*.html", "/**/*.html", "/**/*.css", "/**/*.js", "/webSocket/**").permitAll()
                // swagger 文档
                .antMatchers("/*/api-docs/**", "/swagger-ui/**", "/swagger-resources/**", "/swagger-ui.html", "/webjars/**").permitAll()
                // 文件
                .antMatchers("/avatar/**", "/file/**").permitAll()
                // 阿里巴巴 druid
                .antMatchers("/druid/**").permitAll()
                // 放行OPTIONS请求
                .antMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                // 自定义匿名访问所有url放行：允许匿名和带Token访问，细腻化到每个 Request 类型
                // GET
                .antMatchers(HttpMethod.GET, anonymousUrls.getOrDefault(RequestMethodEnum.GET.getType(), Collections.emptySet()).toArray(new String[0])).permitAll()
                // POST
                .antMatchers(HttpMethod.POST, anonymousUrls.getOrDefault(RequestMethodEnum.POST.getType(), Collections.emptySet()).toArray(new String[0])).permitAll()
                // PUT
                .antMatchers(HttpMethod.PUT, anonymousUrls.getOrDefault(RequestMethodEnum.PUT.getType(), Collections.emptySet()).toArray(new String[0])).permitAll()
                // PATCH
                .antMatchers(HttpMethod.PATCH, anonymousUrls.getOrDefault(RequestMethodEnum.PATCH.getType(), Collections.emptySet()).toArray(new String[0])).permitAll()
                // DELETE
                .antMatchers(HttpMethod.DELETE, anonymousUrls.getOrDefault(RequestMethodEnum.DELETE.getType(), Collections.emptySet()).toArray(new String[0])).permitAll()
                // 所有类型的接口都放行
                .antMatchers(anonymousUrls.getOrDefault(RequestMethodEnum.ALL.getType(), Collections.emptySet()).toArray(new String[0])).permitAll()
                // 所有请求都需要认证
                .anyRequest().authenticated()
                .and().apply(securityConfigurerAdapter());
    }

    private JwtAuthTokenConfigurer securityConfigurerAdapter() {
        return new JwtAuthTokenConfigurer(tokenProvider, userDetailsService);
    }

    private Map<String, Set<String>> getAnonymousUrl(Map<RequestMappingInfo, HandlerMethod> handlerMethodMap) {
        // 不能用instanceof，只能用isInstance()和cast()了
        var pathPatternsClass = PathPatternsRequestCondition.class;
        var patternsClass = PatternsRequestCondition.class;
        // 根据方法类型分组。值为pattern的集合
        return handlerMethodMap.entrySet().parallelStream()
                // 有匿名访问注解
                .filter(infoEntry -> !Objects.isNull(infoEntry.getValue().getMethodAnnotation(AnonymousAccess.class)))
                .flatMap(infoEntry -> {
                    // 先拿到方法类型
                    var requestMethods = new ArrayList<>(infoEntry.getKey().getMethodsCondition().getMethods());
                    var request = RequestMethodEnum.find(requestMethods.isEmpty() ? RequestMethodEnum.ALL.getType() : requestMethods.get(0).name());
                    // 获取pathPatternsCondition
                    var activePatternsCondition = infoEntry.getKey().getActivePatternsCondition();
                    Set<String> patterns;
                    if (pathPatternsClass.isInstance(activePatternsCondition))
                        patterns = pathPatternsClass.cast(activePatternsCondition).getDirectPaths();
                    else if (patternsClass.isInstance(activePatternsCondition))
                        patterns = patternsClass.cast(activePatternsCondition).getPatterns();
                    else
                        throw new IllegalStateException("系统错误，请联系相关人员排查");
                    // 返回一个Stream流，由flatMap进行合并
                    return patterns.stream().map(pattern ->
                            // 二元组。first为methodType，second为pattern
                            Pair.of(request.getType(), pattern)
                    );
                })
                .collect(Collectors.groupingBy(Pair::getFirst, Collectors.mapping(Pair::getSecond, Collectors.toSet())));
    }

    // region loginFilter、handler

    //注册自定义的UsernamePasswordAuthenticationFilter
    @Bean
    CustomAuthenticationFilter customAuthenticationFilter() throws Exception {
        CustomAuthenticationFilter filter = new CustomAuthenticationFilter();
        filter.setAuthenticationSuccessHandler(authenticationSuccessHandler());
        filter.setAuthenticationFailureHandler(authenticationFailureHandler());
        filter.setFilterProcessesUrl("/auth/login");

        //这句很关键，重用WebSecurityConfigurerAdapter配置的AuthenticationManager，不然要自己组装AuthenticationManager
        filter.setAuthenticationManager(authenticationManagerBean());
        return filter;
    }

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
            String token = tokenProvider.createToken(authentication);
            final JwtUserDto jwtUserDto = (JwtUserDto) authentication.getPrincipal();
            // 用户登录成功后，写一条消息
            var authSuccessMsg = new AmqpMsgEntity().setMsgType("auth").setMsgData(jwtUserDto.getUser().toString()).setExtraData("saveAuthorizeLog");
            rabbitMQProducerService.sendMsg(RabbitMqConfig.DIRECT_SYNC_EXCHANGE, RabbitMqConfig.AUTH_LOCAL_ROUTE_KEY, authSuccessMsg);

            // 返回 token 与 用户信息
            Map<String, Object> authInfo = new HashMap<>(2) {
                {
                    put("token", properties.getTokenStartWith() + token);
                    put("user", jwtUserDto);
                }
            };
            // 这里需要进行响应
            ResultUtil.resultJson(response, HttpServletResponse.SC_OK, JsonUtils.toJSONString(authInfo));
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
                if (!Objects.isNull(username)) {
                    var ip = StringUtils.getIp(request);
                    var lockedIp = ip + "||authLocked||";

                    var infoMap = new HashMap<String, Object>();
                    infoMap.put("ip", ip);
                    infoMap.put("username", username);
                    infoMap.put("lockedIp", lockedIp);
                    var authFailedMsg = new AmqpMsgEntity().setMsgType("auth").setMsgData(JsonUtils.toJSONString(infoMap)).setExtraData("solveAuthFailed");
                    //  发送消息
                    rabbitMQProducerService.sendMsg(RabbitMqConfig.DIRECT_SYNC_EXCHANGE, RabbitMqConfig.AUTH_LOCAL_ROUTE_KEY, authFailedMsg);
                }
            }
            // 返回错误信息。用下面的sendError会被EntryPoint拦截并覆盖。
//            response.sendError(HttpServletResponse.SC_BAD_REQUEST, authenticationException.getMessage());
            ResultUtil.resultJson(response, HttpServletResponse.SC_BAD_REQUEST, authenticationException.getMessage());
        };
    }

    // endregion

}
