package com.lwohvye.modules.security.security.filter;

import com.lwohvye.modules.system.service.IResourceService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.SecurityConfig;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.access.intercept.FilterInvocationSecurityMetadataSource;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;

/**
 * @author Hongyan Wang
 * @description 要实现动态配置权限，首先自定义一个类实现FilterInvocationSecurityMetadataSource接口，Spring Security通过接口中的getAttributes方法来确定请求需要哪些角色
 * @date 2021/11/27 2:45 下午
 */
@RequiredArgsConstructor
public class CustomFilterInvocationSecurityMetadataSource implements FilterInvocationSecurityMetadataSource {

    AntPathMatcher antPathMatcher = new AntPathMatcher();  //用来实现ant风格的Url匹配

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
        var filterInvocation = (FilterInvocation) object;
        var requestUrl = filterInvocation.getRequestUrl(); // 获取当前请求的Url
        var reqMethod = filterInvocation.getRequest().getMethod(); // 请求方法GET、POST、PUT、DELETE
        var roleNames = resourceService.queryAllRes().stream() //获取数据库中的所有资源信息，即本案例中的resource以及对应的role
                .filter(resource -> antPathMatcher.match(resource.getPattern(), requestUrl) // URI匹配
                                    && StringUtils.hasText(resource.getRestName()) // 有关联角色（需要特定角色权限）
                                    && Objects.equals(resource.getReqMethod(), reqMethod)) // 请求方法类型匹配
                .flatMap(resource -> Arrays.stream(resource.getRestName().split(","))) // 将字符状态的角色名用逗号切开
                .distinct() // 排重
                .map(roleName -> "ROLE_" + roleName).toList();
        return !roleNames.isEmpty() ? SecurityConfig.createList(roleNames.toArray(String[]::new)) // 构建返回
                : SecurityConfig.createList("ROLE_LOGIN"); //如果请求Url在资源表中不存在相应的模式，则该请求登陆后即可访问
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
     * @param aClass
     * @return
     */
    @Override
    public boolean supports(Class<?> aClass) {
        return true;
    }
}
