/*
 *    Copyright (c) 2022-2025.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.common.web;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.ApiVersionConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * 注册自己实现的mapping匹配规则，这个是在启动时对所有的api进行match（ApiVersionRequestMappingHandlerMapping），满足条件的注入ApiVersionCondition对象，这部分会step into 相关逻辑
 */

@Configuration
public class ApiVersionWebMvcConfigurer implements WebMvcConfigurer {
    // 这里可通过配置文件配置，当前pathSegment需要所有api都适配，还是先用header吧
    @Override
    public void configureApiVersioning(ApiVersionConfigurer configurer) {
        configurer.setVersionRequired(false);
        configurer.useRequestHeader("API-Version");
        // "/api/{version}/xxx" version是1.2.3 major.minor.patch、2.0、3 这种格式，解析时会移除非数字部分
//        configurer.usePathSegment(1);
    }
}
