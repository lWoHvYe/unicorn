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
package com.unicorn;

import com.lwohvye.core.annotation.rest.AnonymousGetMapping;
import com.lwohvye.core.utils.SpringContextHolder;
import com.mzt.logapi.starter.annotation.EnableLogRecord;
import io.swagger.v3.oas.annotations.Hidden;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.bind.annotation.RestController;

/**
 * App启动入口
 */
@EnableAsync // 开启异步
@RestController
@Hidden
@SpringBootApplication(exclude = {TaskExecutionAutoConfiguration.class}) // 核心配置类
@EnableTransactionManagement // 开启事务
@EnableRetry //开启重试机制
@EnableConfigurationProperties //开启 @ConfigurationProperties 注解
@EnableLogRecord(tenant = "com.unicorn.valentineP2P")
public class UnicornAppRun {

    /**
     * Spring Boot入口
     *
     * @param args /
     * @date 2021/11/23 9:43 上午
     */
    public static void main(String[] args) {
        SpringApplication.run(UnicornAppRun.class, args);
    }

    /**
     * 注入Bean对象。用于从ApplicationContext中获取bean实例等
     *
     * @date 2021/11/23 9:42 上午
     */
    @Bean
    public SpringContextHolder springContextHolder() {
        return new SpringContextHolder();
    }

    /**
     * 访问首页提示
     *
     * @return /
     */
    @AnonymousGetMapping("/")
    public String index() {
        return "Unicorn is Running!!";
    }
}
