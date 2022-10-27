/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.config;

import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@ComponentScan("com.lwohvye") // 通过配置这个，启动类的路径不再限制为com.lwohvye or higher
@EnableAspectJAutoProxy // 当启动类不再higher path时，aop会失效，需要显示通过该注解开启
@EnableJpaRepositories(basePackages = {"com.lwohvye.sys.modules.*.repository","com.lwohvye.*.repository"})
@EntityScan(basePackages = {"com.lwohvye.api.modules.*.domain","com.lwohvye.*.domain"})
@EnableJpaAuditing(auditorAwareRef = "auditorAware") // 开启Jpa审计
// @EnableAutoConfiguration // A circular @Import has been detected:
public class UnicornAutoConfiguration {
}
