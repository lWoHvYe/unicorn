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
package com.lwohvye.sys.config.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.lwohvye.config.FileProperties;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;
import org.springframework.web.servlet.config.annotation.*;

import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

/**
 * WebMvcConfigurer
 *
 * @author Zheng Jie
 * @date 2018-11-30
 */
@Configuration
// 在WebMvcAutoConfiguration类上标了一个如下注解：
// @ConditionalOnMissingBean(WebMvcConfigurationSupport.class)
// 以上这行代码的意思就是当前IOC容器中没有WebMvcConfigurationSupport这个类的实例时自动配置类才会生效，这也就是在配置类上标注@EnableWebMvc会导致自动配置类WebMvcAutoConfiguration失效的原因。
// 配置后导致openApi的/v3/api-docs接口返回数据被转义，swagger-ui无法使用。移除遇到的问题已解决
// @EnableWebMvc
public class ConfigurerAdapter implements WebMvcConfigurer {

    /**
     * 文件配置
     */
    private final FileProperties properties;

    public ConfigurerAdapter(FileProperties properties) {
        this.properties = properties;
    }

    // 处理全局跨域

    /**
     * 方法类	            方法名称	               必填	        请求头字段	                     说明
     * CorsRegistry	    addMapping	                是	无, 非Cors属性,属于SpringBoot配置	    配置支持跨域的路径
     * CorsRegistration	allowedOrigins	            是	Access-Control-Allow-Origin	        配置允许的源
     * CorsRegistration	addAllowedOriginPattern	    是	Access-Control-Allow-Origin	        配置允许的源。Spring Boot 2.4.0之后，allowedOrigins不允许使用*，改用这个
     * CorsRegistration	allowedMethods	            是	Access-Control-Allow-Methods	    配置支持跨域请求的方法,如：GET、POST，一次性返回
     * CorsRegistration	maxAge	                    否	Access-Control-Max-Age	            配置预检请求的有效时间
     * CorsRegistration	allowCredentials	        否	Access-Control-Allow-Credentials	配置是否允许发送Cookie, 用于 凭证请求
     * CorsRegistration	allowedHeaders	            否	Access-Control-Request-Headers	    配置允许的自定义请求头, 用于 预检请求
     * CorsRegistration	exposedHeaders	            否	Access-Control-Expose-Headers	    配置响应的头信息,在其中可以设置其他的头信息
     */
    @Bean
    public CorsFilter corsFilter() {
        var source = new UrlBasedCorsConfigurationSource();
        var config = new CorsConfiguration();
        config.setAllowCredentials(true);
        config.addAllowedOriginPattern("*");
        config.addAllowedHeader("*");
        config.addAllowedMethod("*");
        source.registerCorsConfiguration("/**", config);
        return new CorsFilter(source);
    }


    /**
     * 通用拦截器排除设置，所有拦截器都会自动加springdoc-openapi相关的资源排除信息，不用在应用程序自身拦截器定义的地方去添加，算是良心解耦实现。
     */
    @SuppressWarnings("unchecked")
    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        try {
            var registrationsField = FieldUtils.getField(InterceptorRegistry.class, "registrations", true);
            var registrations = (List<InterceptorRegistration>) ReflectionUtils.getField(registrationsField, registry);
            if (registrations != null) {
                for (InterceptorRegistration interceptorRegistration : registrations) {
                    interceptorRegistration.excludePathPatterns("/v3/api-docs/**", "/swagger-ui/**");
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        FileProperties.ElPath path = properties.getOSPath();
        String avatarUtl = "file:" + path.getAvatar().replace("\\", "/");
        String pathUtl = "file:" + path.getPath().replace("\\", "/");
        registry.addResourceHandler("/avatar/**").addResourceLocations(avatarUtl).setCachePeriod(0);
        registry.addResourceHandler("/file/**").addResourceLocations(pathUtl).setCachePeriod(0);
        registry.addResourceHandler("/**").addResourceLocations("classpath:/META-INF/resources/").setCachePeriod(0);
    }

    /**
     * 对向前端返回的数据，日期格式化。
     *
     * @param converters /
     * @date 2021/11/25 10:04 上午
     */
    @Override
    public void configureMessageConverters(List<HttpMessageConverter<?>> converters) {
        var jackson2HttpMessageConverter = new MappingJackson2HttpMessageConverter();
        var supportedMediaTypes = new ArrayList<MediaType>();
        /**
         * Public constant media type for {@code application/json;charset=UTF-8}.
         * @deprecated as of 5.2 in favor of {@link #APPLICATION_JSON}
         * since major browsers like Chrome
         * <a href="https://bugs.chromium.org/p/chromium/issues/detail?id=438464">
         * now comply with the specification</a> and interpret correctly UTF-8 special
         * characters without requiring a {@code charset=UTF-8} parameter.
         */
        supportedMediaTypes.add(MediaType.APPLICATION_JSON);
        jackson2HttpMessageConverter.setSupportedMediaTypes(supportedMediaTypes);
        var jsonMapper = JsonMapper.builder()
                .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
                .defaultDateFormat(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"))
                .serializationInclusion(JsonInclude.Include.NON_NULL)
                .build();
        jsonMapper.registerModule(new JavaTimeModule());
        jackson2HttpMessageConverter.setObjectMapper(jsonMapper);
        jackson2HttpMessageConverter.setDefaultCharset(StandardCharsets.UTF_8);
        converters.add(jackson2HttpMessageConverter);
    }
}
