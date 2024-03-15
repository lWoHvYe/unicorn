/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.sys.modules.security.core.filter;

import com.lwohvye.core.annotation.AnonymousAccess;
import com.lwohvye.core.constant.SecurityConstant;
import com.lwohvye.core.utils.SpringContextHolder;
import com.lwohvye.core.utils.StringUtils;
import com.lwohvye.core.enums.RequestMethodEnum;
import com.lwohvye.sys.modules.security.config.SpringSecurityConfig;
import com.lwohvye.sys.modules.system.service.IResourceService;
import org.springframework.http.server.PathContainer;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.SecurityConfig;
import org.springframework.security.access.SecurityMetadataSource;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.access.intercept.DefaultFilterInvocationSecurityMetadataSource;
import org.springframework.security.web.access.intercept.RequestAuthorizationContext;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.condition.PathPatternsRequestCondition;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.util.pattern.PathPattern;
import org.springframework.web.util.pattern.PathPatternParser;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 要实现动态配置权限，首先自定义一个类实现FilterInvocationSecurityMetadataSource接口，Spring Security通过接口中的getAttributes方法来确定请求需要哪些角色。
 * 在{@link DefaultFilterInvocationSecurityMetadataSource} 中，可获取到配置的requestMap（在{@link SpringSecurityConfig} 中配置的authorizeRequests部分），后续看如何获取这部分信息，
 * <a href="https://docs.spring.io/spring-security/reference/servlet/appendix/faq.html#appendix-faq-dynamic-url-metadata">The first thing you should ask yourself is if you really need to do this.</a>
 *
 * @author Hongyan Wang
 * @date 2021/11/27 2:45 下午
 * @since 6.0 改由AuthorizationManager 调用，不再基于Filter
 */
public final class CustomInvocationSecurityMetadataSource implements SecurityMetadataSource {

    PathMatcher antPathMatcher = new AntPathMatcher();  //用来实现ant风格的Url匹配

    private final IResourceService resourceService;

    private Map<String, List<PathPattern>> anonymousPaths;

    public CustomInvocationSecurityMetadataSource(IResourceService resourceService) {
        this.resourceService = resourceService;
        SpringContextHolder.addCallBacks(this::initAnonymousPaths);
    }


    /**
     * getAttributes 方法确定一个请求需要哪些角色
     *
     * @param object 是FilterInvocation对象，可以获取当前请求的Url
     * @return Collection<ConfigAttribute> 当前请求Url所需要的角色
     * @throws IllegalArgumentException
     */
    @Override
    public Collection<ConfigAttribute> getAttributes(Object object) throws IllegalArgumentException {

        var authorizationContext = (RequestAuthorizationContext) object;
        var url = authorizationContext.getRequest().getRequestURI(); // 获取当前请求的Url
        var httpMethod = authorizationContext.getRequest().getMethod(); // 请求方法GET、POST、PUT、DELETE
        List<ConfigAttribute> attributes;
        // Lookup your database (or other source) using this information and populate the
        // list of attributes
        var securityConfigStream = resourceService.queryAllRes().stream() //获取数据库中的所有资源信息，即本案例中的resource以及对应的role
                .filter(resource -> antPathMatcher.match(resource.getPattern(), url) // URI匹配
                        && !resource.getRoleCodes().isEmpty() // 有关联角色（需要特定角色权限）
                        && (StringUtils.isBlank(resource.getReqMethod()) || Objects.equals(resource.getReqMethod(), httpMethod))) // 请求方法类型匹配。资源未配置请求方法视为全部
                .flatMap(resource -> resource.getRoleCodes().stream()) // 用flatMap合并流
                .distinct() // 排重。到这里，因为是角色级别的，理论上不会太多，排重与否影响不大
                .map(role -> new SecurityConfig(STR."ROLE_\{role.trim()}")); // 需注意，toList的结果是ImmutableCollections
        // 处理匿名注解部分
        var requestPath = PathContainer.parsePath(url);
        var anonymousSecurityConfigStream = Stream.concat(anonymousPaths.getOrDefault(httpMethod, Collections.emptyList()).stream(), // 方法与请求匹配的部分
                        anonymousPaths.getOrDefault(RequestMethodEnum.ALL.getType(), Collections.emptyList()).stream()) // 方法无类型
                .filter(pathPattern -> pathPattern.matches(requestPath)) // 匹配上
                .map(_ -> new SecurityConfig(SecurityConstant.ROLE_ANONYMOUS));
        var securityConfigs = Stream.concat(securityConfigStream, anonymousSecurityConfigStream).toList(); // 合并两股流
        if (!securityConfigs.isEmpty())
            attributes = new ArrayList<>(securityConfigs); // 构建返回
        else
            attributes = Collections.singletonList(new SecurityConfig(SecurityConstant.ROLE_LOGIN)); //如果请求Url在资源表中不存在相应的模式，则该请求登陆后即可访问
        return attributes;
    }

    public void initAnonymousPaths() {
        RequestMappingHandlerMapping requestMappingHandlerMapping = SpringContextHolder.getBean("requestMappingHandlerMapping");
        Map<RequestMappingInfo, HandlerMethod> handlerMethodMap = requestMappingHandlerMapping.getHandlerMethods();
        // 不能用instanceof，只能用isInstance()和cast()了
        var pathPatternsClass = PathPatternsRequestCondition.class;
        var patternsClass = PatternsRequestCondition.class;
        PathPatternParser parser = new PathPatternParser();
        // Local record classes。A record class with components is clearer and safer than an anonymous tuple of implicitly params.
        record PatternMatchCarrier(String methodType, PathPattern pathPattern) {
        }

        // 关于parallelStream，其是使用ForkJoinPool采用分治法来解决问题，池中默认线程为核数-1，需注意提交任务的main线程也参与任务的执行，即实际执行任务的是main线程+池中的线程
        // when using parallelStream + flatMap()，flatMap will reset stream to sequential, so we should use map() + reduce() to concat the stream
        // 根据方法类型分组。值为pattern的集合
        anonymousPaths = handlerMethodMap.entrySet().parallelStream()
                // 有匿名访问注解
                .filter(infoEntry -> !Objects.isNull(infoEntry.getValue().getMethodAnnotation(AnonymousAccess.class)))
                .map(infoEntry -> {
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
                            // Pair.of(request.getType(), parser.parse(pattern))
                            new PatternMatchCarrier(request.getType(), parser.parse(pattern))
                    );
                }).reduce(Stream::concat).orElse(Stream.empty())
                .collect(Collectors.groupingBy(PatternMatchCarrier::methodType, Collectors.mapping(PatternMatchCarrier::pathPattern, Collectors.toUnmodifiableList())));
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
