/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.beans.config.thread;

import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.core.task.support.ContextPropagatingTaskDecorator;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@AutoConfiguration
public class VirtualThreadsPoolExecutor {

    @Bean(name = "taskVTExecutor")
    public ThreadPoolTaskExecutor taskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        // 启用虚拟线程（Java 21 特性）
        executor.setVirtualThreads(true);
        // 使用内置装饰器，它会利用 Context Propagation 库，但更符合 Spring 生命周期
        executor.setTaskDecorator(new ContextPropagatingTaskDecorator());
//        executor.initialize();   // 被Spring管理的Bean可以不写这个
        return executor;
    }
}
