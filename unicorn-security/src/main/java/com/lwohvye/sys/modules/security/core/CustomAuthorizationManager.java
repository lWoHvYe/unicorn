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

package com.lwohvye.sys.modules.security.core;

import com.lwohvye.core.constant.SecurityConstant;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.SecurityMetadataSource;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authorization.AuthorizationDecision;
import org.springframework.security.authorization.AuthorizationManager;
import org.springframework.security.core.Authentication;

import java.util.function.Supplier;

public final class CustomAuthorizationManager<T> implements AuthorizationManager<T> {
    private final SecurityMetadataSource metadata;

    public CustomAuthorizationManager(SecurityMetadataSource securityMetadataSource) {
        this.metadata = securityMetadataSource;
    }

    public AuthorizationDecision check(Supplier<Authentication> authentication, T invocation) {
        var attributes = this.metadata.getAttributes(invocation);

        for (ConfigAttribute configAttribute : attributes) {
            // 使用凭证。访问登录即可访问的资源
            var auth = authentication.get();
            if (SecurityConstant.ROLE_LOGIN.equals(configAttribute.getAttribute()) && auth instanceof UsernamePasswordAuthenticationToken)  //如果请求Url需要的角色是ROLE_LOGIN，说明当前的Url用户登录后即可访问
                return new AuthorizationDecision(true);
            // 访问可匿名访问的资源
            if (SecurityConstant.ROLE_ANONYMOUS.equals(configAttribute.getAttribute())) // 访问匿名资源，放行
                return new AuthorizationDecision(true);
            // 访问受保护的资源。需校验权限
            var auths = auth.getAuthorities(); //获取登录用户具有的角色
            for (var grantedAuthority : auths) {
                if (configAttribute.getAttribute().equals(grantedAuthority.getAuthority())) {
                    return new AuthorizationDecision(true);
                }
            }
        }
        return new AuthorizationDecision(false);
        // return null; // abstain
    }
}
