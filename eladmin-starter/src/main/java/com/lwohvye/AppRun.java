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
package com.lwohvye;

import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.utils.SpringContextHolder;
import io.swagger.v3.oas.annotations.Hidden;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Zheng Jie
 * @description App启动入口
 * @date 2018/11/15 9:20:19
 */
@EnableAsync // 开启异步
@RestController
@Hidden
@SpringBootApplication // 核心配置类
@EnableTransactionManagement // 开启事务
@EnableJpaAuditing(auditorAwareRef = "auditorAware") // 开启Jpa审计
@EnableRetry //开启重试机制
@EnableConfigurationProperties //开启 @ConfigurationProperties 注解
// 以下注解在其他类上有配置
// @EnableCaching
// @EnableOpenApi
// @EnableWebMvc
// @EnableWebSecurity
// @EnableGlobalMethodSecurity(prePostEnabled = true, securedEnabled = true)
public class AppRun {

    /**
     * @param args /
     * @description Spring Boot入口
     * @date 2021/11/23 9:43 上午
     */
    public static void main(String[] args) {
        SpringApplication.run(AppRun.class, args);
    }

    /**
     * @description 注入Bean对象。用于从ApplicationContext中获取bean实例等
     * @date 2021/11/23 9:42 上午
     */
    @Bean
    public SpringContextHolder springContextHolder() {
        return new SpringContextHolder();
    }

    /**
     * @return /
     * @description 访问首页提示
     */
    @AnonymousGetMapping("/")
    public String index() {
        return "Backend service started successfully";
    }
}
