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
package com.lwohvye.core.utils;

import com.lwohvye.core.exception.BadRequestException;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.oauth2.jwt.Jwt;

/**
 * 获取当前登录的用户
 *
 * @author Zheng Jie
 * @date 2019-01-17
 */
@Slf4j
@UtilityClass
public class SecurityUtils {

    /**
     * 获取当前登录的用户
     *
     * @return UserDetails
     */
    public static UserDetails getCurrentUser() {
        UserDetailsService userDetailsService = SpringContextHolder.getBean(UserDetailsService.class);
        return userDetailsService.loadUserByUsername(getCurrentUsername());
    }

    /**
     * 获取系统用户名称
     *
     * @return 系统用户名称
     */
    public static String getCurrentUsername() {
        final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null)
            throw new BadRequestException(HttpStatus.UNAUTHORIZED, "当前登录状态过期");
        var principal = authentication.getPrincipal();
        if (principal instanceof UserDetails userDetails)
            return userDetails.getUsername();
        if (principal instanceof Jwt jwt)
            return jwt.getSubject();
        throw new BadRequestException(HttpStatus.UNAUTHORIZED, "找不到当前登录的信息");
    }

    /**
     * 获取系统用户ID
     *
     * @return 系统用户ID
     */
    public static Long getCurrentUserId() {
        return extractUserId(getCurrentUser());
    }

    /**
     * 从 UserDetails 中读取嵌套的 user.id，避免把整个对象序列化成 JSON 再取字段。
     * 使用 BeanWrapper 而不是直接依赖 security 模块的 JwtUserDto，保持 core 模块解耦。
     */
    static Long extractUserId(UserDetails userDetails) {
        var wrapper = new BeanWrapperImpl(userDetails);
        if (!wrapper.isReadableProperty("user.id")) {
            throw new BadRequestException(HttpStatus.UNAUTHORIZED, "当前用户信息缺少 user.id");
        }
        var value = wrapper.getPropertyValue("user.id");
        if (value instanceof Number number) {
            return number.longValue();
        }
        if (value instanceof CharSequence text) {
            try {
                return Long.valueOf(text.toString());
            } catch (NumberFormatException e) {
                throw new BadRequestException(HttpStatus.UNAUTHORIZED, "当前用户 user.id 格式不正确");
            }
        }
        throw new BadRequestException(HttpStatus.UNAUTHORIZED, "当前用户信息缺少 user.id");
    }
}
