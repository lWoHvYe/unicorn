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
package com.lwohvye.tools.config

import jakarta.servlet.MultipartConfigElement
import org.springframework.boot.web.servlet.MultipartConfigFactory
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import java.io.File

/**
 * @date 2018-12-28
 * @author https://blog.csdn.net/llibin1024530411/article/details/79474953
 */
@Configuration
class MultipartConfig {
    /**
     * 文件上传临时路径
     * 若未配置临时路径，当上传大文件时会报错。
     * 配置该属性后，以下限制无效。大致原因是这个是tomcat的，配置临时目录后临时文件不放到tomcat中
     * spring:
     * servlet:
     * multipart:
     * file-size-threshold: 2KB
     * max-file-size: 100MB
     * max-request-size: 200MB
     */
    @Bean
    fun multipartConfigElement(): MultipartConfigElement {
        val factory = MultipartConfigFactory()
        val location = System.getProperty("user.home") + "/.unicorn/file/tmp"
        val tmpFile = File(location)
        if (!tmpFile.exists() && !tmpFile.mkdirs()) {
            println("create was not successful.")
        }
        factory.setLocation(location)
        return factory.createMultipartConfig()
    }
}
