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
package com.lwohvye.config;

import com.fasterxml.classmate.TypeResolver;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.data.domain.Pageable;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.servlet.mvc.method.RequestMappingInfoHandlerMapping;
import org.springframework.web.util.pattern.PathPatternParser;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestParameterBuilder;
import springfox.documentation.oas.annotations.EnableOpenApi;
import springfox.documentation.schema.AlternateTypeRule;
import springfox.documentation.schema.AlternateTypeRuleConvention;
import springfox.documentation.service.*;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spi.service.contexts.SecurityContext;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.spring.web.plugins.WebFluxRequestHandlerProvider;
import springfox.documentation.spring.web.plugins.WebMvcRequestHandlerProvider;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import static com.google.common.collect.Lists.newArrayList;
import static java.util.stream.Collectors.joining;
import static springfox.documentation.schema.AlternateTypeRules.newRule;

/**
 * @author Zheng Jie
 * @description api页面 /doc.html /swagger-ui/index.html
 * @date 2018-11-23
 */
@Slf4j
@Configuration
@EnableOpenApi
public class SwaggerConfig {

    @Value("${jwt.header:Authorization}")
    private String tokenHeader;

    @Value("${swagger.enabled:true}")
    private Boolean enabled;

    @Bean
    @SuppressWarnings("all")
    public Docket createRestApi() {
        // 可以通过RequestParameter定义些公共的请求参数，以下只是实例
        var ticketPar = new RequestParameterBuilder();
        var pars = new ArrayList<RequestParameter>();
        ticketPar.name("platform")
                .description("平台信息（标识）")
                // 这里设置放到请求头里
                .in(ParameterType.HEADER)
                .required(false)
                .build();
        pars.add(ticketPar.build());
        // ------------------------公共参数定义结束-------------------------------
        return new Docket(DocumentationType.SWAGGER_2)
                .enable(enabled)
                .pathMapping("/")
//                .groupName("核心Api文档")
                .apiInfo(apiInfo())
                .select()
//                api可以分组。下面指定了该组对应的package路径
//                .apis(RequestHandlerSelectors.basePackage("com.lwohvye.modules"))
                .paths(PathSelectors.any())
                .build()
                //添加登陆认证
                .securitySchemes(securitySchemes())
                .securityContexts(securityContexts())
                // 设置公共参数
                .globalRequestParameters(pars);
    }

    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .description("1101100_1010111_1101111_1001000_1110110_1011001_1100101")
                .title("王红岩- -lWoHvYe")
                .version("2.6.17")
                .build();
    }

    // 授权信息
    private List<SecurityScheme> securitySchemes() {
        //设置请求头信息
        var securitySchemes = new ArrayList<SecurityScheme>();
        var apiKey = new ApiKey(tokenHeader, tokenHeader, "header");
        securitySchemes.add(apiKey);
        return securitySchemes;
    }

    // 授权信息应用到上下文
    private List<SecurityContext> securityContexts() {
        //设置需要登录认证的路径
        List<SecurityContext> securityContexts = new ArrayList<>();
        // ^(?!auth).*$ 表示所有包含auth的接口不需要使用securitySchemes即不需要带token
        // ^标识开始  ()里是一子表达式  ?!/auth表示匹配不是/auth的位置，匹配上则添加请求头，注意路径已/开头  .表示任意字符  *表示前面的字符匹配多次 $标识结束
        // Ant URI相关参考：https://www.lwohvye.com/2021/11/27/spring-mvc-%e8%b7%af%e5%be%84uri%e4%b8%ad%e7%9a%84-ant-%e9%a3%8e%e6%a0%bc/
        securityContexts.add(getContextByPath());
        return securityContexts;
    }

    private SecurityContext getContextByPath() {
        return SecurityContext.builder()
                .securityReferences(defaultAuth())
                .forPaths(PathSelectors.regex("^(?!/auth).*$"))
                .build();
    }

    private List<SecurityReference> defaultAuth() {
        List<SecurityReference> securityReferences = new ArrayList<>();
        AuthorizationScope authorizationScope = new AuthorizationScope("global", "accessEverything");
        AuthorizationScope[] authorizationScopes = new AuthorizationScope[1];
        authorizationScopes[0] = authorizationScope;
        securityReferences.add(new SecurityReference(tokenHeader, authorizationScopes));
        return securityReferences;
    }

    /**
     * @return org.springframework.beans.factory.config.BeanPostProcessor
     * @description 针对springfox在Spring Boot 2.6系列版本不兼容，一个解决方案。在bean初始化前后调用该方法，后续还可以做些别的事情
     * <a href="https://github.com/springfox/springfox/issues/3462#issuecomment-983144080">解决方案</a>
     * @date 2021/12/1 9:07 上午
     */
    @Bean
    public static BeanPostProcessor springfoxHandlerProviderBeanPostProcessor() {
        return new BeanPostProcessor() {

            // 该方法在bean初始化之后调用。还有一个Before在初始化之前调用
            @Override
            public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
                // 只针对这两类处理
                if (bean instanceof WebMvcRequestHandlerProvider || bean instanceof WebFluxRequestHandlerProvider)
                    customizeSpringfoxHandlerMappings(getHandlerMappings(bean));
                return bean;
            }

            private <T extends RequestMappingInfoHandlerMapping> void customizeSpringfoxHandlerMappings(List<T> mappings) {
                var warring = """    
                          The actual matchingStrategy of the bean [{}] is [PathPatternParser], which is not support by springfox, ignored.
                            It contains the following patterns [{}].
                            Notice that you have to set `spring.mvc.pathmatch.matching-strategy=ant-path-matcher` in the configuration.
                          see: https://github.com/spring-projects/spring-boot/wiki/Spring-Boot-2.6-Release-Notes
                        """;
                List<T> copy = mappings.stream()
                        .filter(mapping -> {
                            // mapping.getPatternParser() Return the configured PathPatternParser, or null。把无PatternsRequestCondition的过滤掉了。
                            PathPatternParser patternParser = mapping.getPatternParser();
                            if (!Objects.isNull(patternParser)) {
                                String beanName = mapping.getClass().getSimpleName();
                                String patterns = mapping.getHandlerMethods().keySet().stream()
                                        .flatMap(requestMappingInfo -> requestMappingInfo.getPathPatternsCondition() != null ?
                                                requestMappingInfo.getPathPatternsCondition().getDirectPaths().stream() : Stream.empty())
                                        .collect(joining(","));
                                log.warn(warring, beanName, patterns);
                            }
                            return Objects.isNull(patternParser);
                        }).toList();
                mappings.clear();
                mappings.addAll(copy);
            }

            @SuppressWarnings("unchecked")
            private List<RequestMappingInfoHandlerMapping> getHandlerMappings(Object bean) {
                try {
                    // 获取属性
                    Field field = ReflectionUtils.findField(bean.getClass(), "handlerMappings");
                    // 改变属性的访问状态
                    ReflectionUtils.makeAccessible(field);
                    return (List<RequestMappingInfoHandlerMapping>) field.get(bean);
                } catch (IllegalArgumentException | IllegalAccessException e) {
                    throw new IllegalStateException(e);
                }
            }
        };
    }

}

/**
 * 将Pageable转换展示在swagger中
 */
@Configuration
class SwaggerDataConfig {

    @Bean
    public AlternateTypeRuleConvention pageableConvention(final TypeResolver resolver) {
        return new AlternateTypeRuleConvention() {
            @Override
            public int getOrder() {
                return Ordered.HIGHEST_PRECEDENCE;
            }

            @Override
            public List<AlternateTypeRule> rules() {
                return newArrayList(newRule(resolver.resolve(Pageable.class), resolver.resolve(Page.class)));
            }
        };
    }

    @ApiModel
    @Data
    private static class Page {
        @ApiModelProperty("页码 (0..N)")
        private Integer page;

        @ApiModelProperty("每页显示的数目")
        private Integer size;

        @ApiModelProperty("以下列格式排序标准：property[,asc | desc]。 默认排序顺序为升序。 支持多种排序条件：如：id,asc")
        private List<String> sort;
    }
}
