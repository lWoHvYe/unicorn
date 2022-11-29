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

import org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration;
import org.springframework.boot.web.embedded.tomcat.TomcatProtocolHandlerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.core.task.support.TaskExecutorAdapter;

import java.util.concurrent.Executors;

/**
 * Running Spring Applications on Virtual Threads
 * <a href="https://spring.io/blog/2022/10/11/embracing-virtual-threads">embracing virtual threads</a>
 */
public class WebExecutorConfig {

    @Bean(TaskExecutionAutoConfiguration.APPLICATION_TASK_EXECUTOR_BEAN_NAME)
    public AsyncTaskExecutor asyncTaskExecutor() {
        var virtualFactory = Thread.ofVirtual().name("Virtual-Async").factory();
        return new TaskExecutorAdapter(Executors.newThreadPerTaskExecutor(virtualFactory));
    }

    @Bean
    public TomcatProtocolHandlerCustomizer<?> protocolHandlerVirtualThreadExecutorCustomizer() {
        var virtualFactory = Thread.ofVirtual().name("Virtual-WebServer").factory();
        return protocolHandler -> protocolHandler.setExecutor(Executors.newThreadPerTaskExecutor(virtualFactory));
    }
}
