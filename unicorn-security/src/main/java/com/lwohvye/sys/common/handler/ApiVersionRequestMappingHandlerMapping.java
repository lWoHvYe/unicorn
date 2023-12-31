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

package com.lwohvye.sys.common.handler;

import com.lwohvye.sys.common.annotation.ApiVersion;
import com.lwohvye.sys.common.condition.ApiVersionCondition;
import org.jetbrains.annotations.NotNull;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.web.servlet.mvc.condition.RequestCondition;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import java.lang.reflect.Method;
import java.util.Objects;

/**
 * <a href="https://blog.csdn.net/weixin_39255905/article/details/110391515">...</a>
 * 自定义匹配的处理器
 * 这里用了JDK 18的一个特性：JEP 413:	Code Snippets in Java API Documentation
 * {@snippet :
 *
 *    @ApiVersion
 *    @GetMapping("/{version}/test1") // 这种在`ApiVersion`中不指定version的，适合各个版本，因为默认值是1所以是 [1+
 *    @ResponseBody
 *    public String test1(){
 * 		return "test 1";
 *    }
 *    @ApiVersion
 *    @GetMapping("/{ver}/test2") // 这里的占位符不只限`version`
 *    @ResponseBody
 *    public String test2(){
 * 		return "test 2";
 *    }
 *    @ApiVersion(3)
 *    @GetMapping("/{version}/test3") // 这种指定了version的，表示的从这个版本开始 [3+
 *    @ResponseBody
 *    public String test3(){
 * 		return "test 3";
 *    }
 * }
 */
public class ApiVersionRequestMappingHandlerMapping extends RequestMappingHandlerMapping {

    @Override
    protected RequestCondition<ApiVersionCondition> getCustomTypeCondition(@NotNull Class<?> handlerType) {
        var apiVersion = AnnotationUtils.findAnnotation(handlerType, ApiVersion.class);
        return createCondition(apiVersion);
    }

    @Override
    protected RequestCondition<ApiVersionCondition> getCustomMethodCondition(@NotNull Method method) {
        var apiVersion = AnnotationUtils.findAnnotation(method, ApiVersion.class);
        return createCondition(apiVersion);
    }

    private RequestCondition<ApiVersionCondition> createCondition(ApiVersion apiVersion) {
        return Objects.isNull(apiVersion) ? null : new ApiVersionCondition(apiVersion.value());
    }
}
