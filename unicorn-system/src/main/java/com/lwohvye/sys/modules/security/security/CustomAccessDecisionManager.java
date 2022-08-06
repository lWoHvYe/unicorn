/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.sys.modules.security.security;

import com.lwohvye.constant.SecurityConstant;
import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;

import java.util.Collection;

/**
 * @author Hongyan Wang
 * @description
 * @date 2021/11/27 3:46 下午
 */
public class CustomAccessDecisionManager implements AccessDecisionManager {
    /**
     * 判断登录的用户是否具有请求Url所需要的角色信息，如果没有，就抛出AccessDeniedException异常
     *
     * @param auth   包含当前登录用户的信息
     * @param object 获取当前请求对象
     * @param ca     FilterInnovation...中getAttributes的返回值,即当前请求所需要的角色
     * @throws AccessDeniedException
     * @throws InsufficientAuthenticationException
     */
    @Override
    public void decide(Authentication auth, Object object, Collection<ConfigAttribute> ca) throws AccessDeniedException, InsufficientAuthenticationException {
        for (ConfigAttribute configAttribute : ca) {
            // 使用凭证。访问登录即可访问的资源
            if (SecurityConstant.ROLE_LOGIN.equals(configAttribute.getAttribute()) && auth instanceof UsernamePasswordAuthenticationToken)  //如果请求Url需要的角色是ROLE_LOGIN，说明当前的Url用户登录后即可访问
                return;
            // 访问可匿名访问的资源
            if (SecurityConstant.ROLE_ANONYMOUS.equals(configAttribute.getAttribute())) // 访问匿名资源，放行
                return;
            // 访问受保护的资源。需校验权限
            var auths = auth.getAuthorities(); //获取登录用户具有的角色
            for (GrantedAuthority grantedAuthority : auths) {
                if (configAttribute.getAttribute().equals(grantedAuthority.getAuthority())) {
                    return;
                }
            }
        }
        throw new AccessDeniedException("权限不足");
    }

    @Override
    public boolean supports(ConfigAttribute configAttribute) {
        return true;
    }

    @Override
    public boolean supports(Class<?> aClass) {
        return true;
    }
}

