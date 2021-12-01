/*
 *  Copyright 2020-2022 lWoHvYe
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
package com.lwohvye.modules.security.security.filter;

import com.lwohvye.constant.SecurityConstant;
import com.lwohvye.modules.security.config.SpringSecurityConfig;
import com.lwohvye.modules.system.service.IResourceService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.SecurityConfig;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.access.intercept.DefaultFilterInvocationSecurityMetadataSource;
import org.springframework.security.web.access.intercept.FilterInvocationSecurityMetadataSource;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;

import java.util.*;

/**
 * @author Hongyan Wang
 * @description 要实现动态配置权限，首先自定义一个类实现FilterInvocationSecurityMetadataSource接口，Spring Security通过接口中的getAttributes方法来确定请求需要哪些角色。
 * 在{@link DefaultFilterInvocationSecurityMetadataSource} 中，可获取到配置的requestMap（在{@link SpringSecurityConfig} 中配置的authorizeRequests部分），后续看如何获取这部分信息，
 * <a href="https://docs.spring.io/spring-security/site/docs/4.2.4.RELEASE/reference/htmlsingle/#appendix-faq-dynamic-url-metadata">The first thing you should ask yourself is if you really need to do this.</a>
 * @date 2021/11/27 2:45 下午
 */
@RequiredArgsConstructor
public class CustomFilterInvocationSecurityMetadataSource implements FilterInvocationSecurityMetadataSource {

    PathMatcher antPathMatcher = new AntPathMatcher();  //用来实现ant风格的Url匹配

    private final IResourceService resourceService;

    /**
     * getAttributes 方法确定一个请求需要哪些角色
     *
     * @param object 是FilterInvocation对象，可以获取当前请求的Url
     * @return Collection<ConfigAttribute> 当前请求Url所需要的角色
     * @throws IllegalArgumentException
     */
    @Override
    public Collection<ConfigAttribute> getAttributes(Object object) throws IllegalArgumentException {

        var fi = (FilterInvocation) object;
        var url = fi.getRequestUrl(); // 获取当前请求的Url
        var httpMethod = fi.getRequest().getMethod(); // 请求方法GET、POST、PUT、DELETE
        List<ConfigAttribute> attributes;
        // Lookup your database (or other source) using this information and populate the
        // list of attributes
        var securityConfigs = resourceService.queryAllRes().stream() //获取数据库中的所有资源信息，即本案例中的resource以及对应的role
                .filter(resource -> antPathMatcher.match(resource.getPattern(), url) // URI匹配
                                    && !resource.getRoleCodes().isEmpty() // 有关联角色（需要特定角色权限）
                                    && (Objects.isNull(resource.getReqMethod()) || Objects.equals(resource.getReqMethod(), httpMethod))) // 请求方法类型匹配。资源未配置请求方法视为全部
                .flatMap(resource -> resource.getRoleCodes().stream()) // 用flatMap合并流
                .distinct() // 排重。到这里，因为是角色级别的，理论上不会太多，排重与否影响不大
                .map(role -> new SecurityConfig("ROLE_" + role.trim())).toList();
        if (!securityConfigs.isEmpty())
            attributes = new ArrayList<>(securityConfigs); // 构建返回
        else attributes = Collections.singletonList(new SecurityConfig(SecurityConstant.ROLE_LOGIN)); //如果请求Url在资源表中不存在相应的模式，则该请求登陆后即可访问
        return attributes;
    }

    /**
     * @return 返回所有定义好的权限资源，Spring Security启动时会校验相关配置是否正确，如果不需要校验，直接返回null即可
     */
    @Override
    public Collection<ConfigAttribute> getAllConfigAttributes() {
        return null;
    }

    /**
     * 返回类对象是否支持校验
     *
     * @param clazz
     * @return
     */
    @Override
    public boolean supports(Class<?> clazz) {
        return FilterInvocation.class.isAssignableFrom(clazz);
    }
}
