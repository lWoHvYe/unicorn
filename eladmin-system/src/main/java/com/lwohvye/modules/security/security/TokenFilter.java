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

import com.lwohvye.modules.security.config.bean.SecurityProperties;
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
public class TokenFilter extends GenericFilterBean {
    private static final Logger log = LoggerFactory.getLogger(TokenFilter.class);


    private final TokenProvider tokenProvider; // Token
    private final SecurityProperties properties; // Jwt
    private final UserDetailsService userDetailsService;


    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {
        HttpServletRequest httpServletRequest = (HttpServletRequest) servletRequest;
        String token = resolveToken(httpServletRequest);
        // 对于 Token 为空的不需要去查 Redis
        if (StringUtils.hasText(token)) {
//            OnlineUserDto onlineUserDto = null;
//            boolean cleanUserCache = false;
            try {
                // 获取用户基础信息(从token中)
                Authentication authentication = tokenProvider.getAuthentication(token);
                if (authentication.getPrincipal() instanceof UserDetails userDetails) {
                    // 根据用户名，从服务侧获取用户详细信息
                    var jwtUserDto = (JwtUserDto) userDetailsService.loadUserByUsername(userDetails.getUsername());
                    // 校验
                    if (Boolean.TRUE.equals(tokenProvider.validateToken(token, jwtUserDto))) {
                        // 是否刷新了token
                        var newToken = tokenProvider.refreshToken(token);
                        if (StringUtils.hasLength(newToken)) {
                            // TODO: 2021/11/11 将新的token返回前端，并在前端更新
                        }

                        SecurityContextHolder.getContext().setAuthentication(authentication);
                    }
                }
//                onlineUserDto = onlineUserService.getOne(SecuritySysUtil.getAuthToken(properties, token));
            } catch (ExpiredJwtException e) {
                log.error(e.getMessage());
//                cleanUserCache = true;
            } finally {
//                if (cleanUserCache || Objects.isNull(onlineUserDto)) {
//                    userCacheClean.cleanUserCache(String.valueOf(tokenProvider.getClaims(token).get(TokenProvider.AUTHORITIES_KEY)));
//                }
            }
//            if (onlineUserDto != null && StringUtils.hasText(token)) {
//                Authentication authentication = tokenProvider.getAuthentication(token);
//                SecurityContextHolder.getContext().setAuthentication(authentication);
//                 Token 续期
//                tokenProvider.checkRenewal(token);
//            }
        }
        filterChain.doFilter(servletRequest, servletResponse);
    }

    /**
     * 初步检测Token
     *
     * @param request /
     * @return /
     */
    private String resolveToken(HttpServletRequest request) {
        String bearerToken = request.getHeader(properties.getHeader());
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith(properties.getTokenStartWith())) {
            // 去掉令牌前缀
            return bearerToken.replace(properties.getTokenStartWith(), "");
        } else {
            log.debug("非法Token：{}", bearerToken);
        }
        return null;
    }
}
