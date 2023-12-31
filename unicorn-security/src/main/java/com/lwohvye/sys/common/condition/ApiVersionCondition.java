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

package com.lwohvye.sys.common.condition;

import org.jetbrains.annotations.NotNull;
import org.springframework.web.servlet.mvc.condition.RequestCondition;

import jakarta.servlet.http.HttpServletRequest;
import java.util.regex.Pattern;

/**
 * 自定义url匹配逻辑
 * 版本号筛选，将提取请求URL中版本号，与注解上定义的版本号进行比对，以此来判断某个请求应落在哪个controller上。
 */
public record ApiVersionCondition(int apiVersion) implements RequestCondition<ApiVersionCondition> {

    // 路径中版本的前缀， 这里用 /v[1-9]/的形式，前后都要有`/`即，version属性是在中间的，应该不会有放到末尾的吧
    private static final Pattern VERSION_PREFIX_PATTERN = Pattern.compile("v(\\d+)/");

    /**
     * 当方法级别和类级别都有ApiVersion注解时，二者将进行合并（ApiVersionRequestCondition.combine）
     */
    @Override
    public @NotNull ApiVersionCondition combine(ApiVersionCondition apiVersionCondition) {
        // 采用最后定义优先原则，则方法上的定义覆盖类上面的定义
        return new ApiVersionCondition(apiVersionCondition.apiVersion());
    }

    @Override
    public ApiVersionCondition getMatchingCondition(HttpServletRequest httpServletRequest) {
        var m = VERSION_PREFIX_PATTERN.matcher(httpServletRequest.getRequestURI());
        if (m.find()) {
            var version = Integer.parseInt(m.group(1));
            if (version >= this.apiVersion) { // 这里限定了实际版本（注解中标记的）要不大于指定的版本（url来的）
                return this;
            }
        }
        return null;
    }

    @Override
    public int compareTo(ApiVersionCondition apiVersionCondition, @NotNull HttpServletRequest httpServletRequest) {
        // 优先匹配最新的版本号
        return apiVersionCondition.apiVersion() - this.apiVersion;
    }
}
