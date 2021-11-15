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
package com.lwohvye.modules.security.security;

import com.lwohvye.modules.security.service.dto.JwtUserDto;
import io.jsonwebtoken.ExpiredJwtException;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.GenericFilterBean;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

/**
 * @author /
 */
@RequiredArgsConstructor
public class JwtAuthenticationTokenFilter extends GenericFilterBean {
    private static final Logger log = LoggerFactory.getLogger(JwtAuthenticationTokenFilter.class);


    private final TokenProvider tokenProvider; // Token
    private final UserDetailsService userDetailsService; // UserDetails


    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {
        HttpServletRequest httpServletRequest = (HttpServletRequest) servletRequest;
        String token = tokenProvider.getToken(httpServletRequest);
        // 对于 Token 为空的不需要去查 Redis
        if (StringUtils.hasText(token)) {
            try {
                // 获取用户基础信息(从token中)
                Authentication authentication = tokenProvider.getAuthentication(token);
                if (authentication.getPrincipal() instanceof UserDetails userDetails) {
                    // 根据用户名，从服务侧获取用户详细信息
                    var jwtUserDto = (JwtUserDto) userDetailsService.loadUserByUsername(userDetails.getUsername());
                    // 校验
                    if (Boolean.TRUE.equals(tokenProvider.validateToken(token, jwtUserDto))) {
                        // 剩余时常不足时，进行通知（只通知一次）
                        tokenProvider.noticeExpire5Token(token);
                        SecurityContextHolder.getContext().setAuthentication(authentication);
                    }
                }
            } catch (ExpiredJwtException e) {
                log.error(e.getMessage());
            }
        }
        filterChain.doFilter(servletRequest, servletResponse);
    }

}
