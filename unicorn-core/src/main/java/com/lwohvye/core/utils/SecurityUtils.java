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
import com.lwohvye.core.utils.enums.DataScopeEnum;
import com.lwohvye.core.utils.json.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

/**
 * 获取当前登录的用户
 *
 * @author Zheng Jie
 * @date 2019-01-17
 */
@Slf4j
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
        throw new BadRequestException(HttpStatus.UNAUTHORIZED, "找不到当前登录的信息");
    }

    /**
     * 获取系统用户ID
     *
     * @return 系统用户ID
     */
    public static Long getCurrentUserId() {
        UserDetails userDetails = getCurrentUser();
        return JsonUtils.findPath(userDetails, "user", "id", Long.class);
    }

    /**
     * 获取当前用户的数据权限
     *
     * @return /
     */
    public static String getCurrentUserDataScope() {
        UserDetails userDetails = getCurrentUser();
        return JsonUtils.findPath(userDetails, "dataScope", null, String.class);
    }

    /**
     * 获取数据权限级别
     *
     * @return 级别
     */
    public static String getDataScopeType() {
        var dataScope = getCurrentUserDataScope();
        if (StringUtils.isNotBlank(dataScope)) {
            return dataScope;
        }
        return DataScopeEnum.ALL.getValue();
    }
}
