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
import com.lwohvye.modules.security.security.JwtAccessDeniedHandler;
import com.lwohvye.modules.security.security.JwtAuthTokenConfigurer;
import com.lwohvye.modules.security.security.JwtAuthenticationEntryPoint;
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
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.filter.CorsFilter;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import javax.servlet.http.HttpServletResponse;
import java.util.*;

/**
 * @author Zheng Jie
 */
@Configuration
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
                .csrf().disable()
                //用重写的Filter替换掉原有的UsernamePasswordAuthenticationFilter（这里实际上是放到了前面，security自带的Filter在轮到自己执行的时候，会判断当前登录状态，如果已经被之前的Filter验证过了，自己这关就直接放行）
                .addFilterAt(customAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class)
                .addFilterBefore(corsFilter, UsernamePasswordAuthenticationFilter.class)
                // 授权异常
                .exceptionHandling()
                .authenticationEntryPoint(authenticationErrorHandler)
                .accessDeniedHandler(jwtAccessDeniedHandler)
                // 防止iframe 造成跨域
                .and()
                .headers()
                .frameOptions()
                .disable()
                // 不创建会话
                .and()
                .sessionManagement()
                .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                .and()
                .authorizeRequests()
                // 静态资源等等
                .antMatchers(
                        HttpMethod.GET,
                        "/*.html",
                        "/**/*.html",
                        "/**/*.css",
                        "/**/*.js",
                        "/webSocket/**"
                ).permitAll()
                // swagger 文档
                .antMatchers("/swagger-ui.html").permitAll()
                .antMatchers("/swagger-resources/**").permitAll()
                .antMatchers("/webjars/**").permitAll()
                .antMatchers("/*/api-docs").permitAll()
                // 文件
                .antMatchers("/avatar/**").permitAll()
                .antMatchers("/file/**").permitAll()
                // 阿里巴巴 druid
                .antMatchers("/druid/**").permitAll()
                // 放行OPTIONS请求
                .antMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                // 自定义匿名访问所有url放行：允许匿名和带Token访问，细腻化到每个 Request 类型
                // GET
                .antMatchers(HttpMethod.GET, anonymousUrls.get(RequestMethodEnum.GET.getType()).toArray(new String[0])).permitAll()
                // POST
                .antMatchers(HttpMethod.POST, anonymousUrls.get(RequestMethodEnum.POST.getType()).toArray(new String[0])).permitAll()
                // PUT
                .antMatchers(HttpMethod.PUT, anonymousUrls.get(RequestMethodEnum.PUT.getType()).toArray(new String[0])).permitAll()
                // PATCH
                .antMatchers(HttpMethod.PATCH, anonymousUrls.get(RequestMethodEnum.PATCH.getType()).toArray(new String[0])).permitAll()
                // DELETE
                .antMatchers(HttpMethod.DELETE, anonymousUrls.get(RequestMethodEnum.DELETE.getType()).toArray(new String[0])).permitAll()
                // 所有类型的接口都放行
                .antMatchers(anonymousUrls.get(RequestMethodEnum.ALL.getType()).toArray(new String[0])).permitAll()
                // 所有请求都需要认证
                .anyRequest().authenticated()
                .and().apply(securityConfigurerAdapter());
    }

    private JwtAuthTokenConfigurer securityConfigurerAdapter() {
        return new JwtAuthTokenConfigurer(tokenProvider, userDetailsService);
    }

    private Map<String, Set<String>> getAnonymousUrl(Map<RequestMappingInfo, HandlerMethod> handlerMethodMap) {
        Map<String, Set<String>> anonymousUrls = new HashMap<>(6);
        Set<String> get = new HashSet<>();
        Set<String> post = new HashSet<>();
        Set<String> put = new HashSet<>();
        Set<String> patch = new HashSet<>();
        Set<String> delete = new HashSet<>();
        Set<String> all = new HashSet<>();
        for (Map.Entry<RequestMappingInfo, HandlerMethod> infoEntry : handlerMethodMap.entrySet()) {
            HandlerMethod handlerMethod = infoEntry.getValue();
            AnonymousAccess anonymousAccess = handlerMethod.getMethodAnnotation(AnonymousAccess.class);
            if (null != anonymousAccess) {
                List<RequestMethod> requestMethods = new ArrayList<>(infoEntry.getKey().getMethodsCondition().getMethods());
                RequestMethodEnum request = RequestMethodEnum.find(requestMethods.isEmpty() ? RequestMethodEnum.ALL.getType() : requestMethods.get(0).name());
                var patternsCondition = infoEntry.getKey().getPatternsCondition();
                Assert.notNull(patternsCondition, "系统错误，请联系相关人员排查");
                var patterns = patternsCondition.getPatterns();
                switch (Objects.requireNonNull(request)) {
                    case GET -> get.addAll(patterns);
                    case POST -> post.addAll(patterns);
                    case PUT -> put.addAll(patterns);
                    case PATCH -> patch.addAll(patterns);
                    case DELETE -> delete.addAll(patterns);
                    default -> all.addAll(patterns);
                }
            }
        }
        anonymousUrls.put(RequestMethodEnum.GET.getType(), get);
        anonymousUrls.put(RequestMethodEnum.POST.getType(), post);
        anonymousUrls.put(RequestMethodEnum.PUT.getType(), put);
        anonymousUrls.put(RequestMethodEnum.PATCH.getType(), patch);
        anonymousUrls.put(RequestMethodEnum.DELETE.getType(), delete);
        anonymousUrls.put(RequestMethodEnum.ALL.getType(), all);
        return anonymousUrls;
    }

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

}
