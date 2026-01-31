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
import org.springframework.core.task.TaskDecorator;
import org.springframework.core.task.support.ContextPropagatingTaskDecorator;

@AutoConfiguration
public class VirtualExecutorConfig {

    // 在 Spring Boot 4 中，只要你定义了 TaskDecorator Bean，默认的 applicationTaskExecutor 就会自动应用它，保证 @Async 和其他自动配置的 Executor 都能传播上下文，无需额外包装。
    @Bean
    public TaskDecorator contextPropagatingTaskDecorator() {
        return new ContextPropagatingTaskDecorator(); // 你的实现
    }

/*    // applicationTaskExecutor 是 Spring Boot 提供的 全局默认异步任务线程池，它是 @Async 的默认 Executor
    @Bean
    public Executor customVirtualExecutor(@Qualifier(TaskExecutionAutoConfiguration.APPLICATION_TASK_EXECUTOR_BEAN_NAME) Executor defaultExecutor, TaskDecorator contextPropagatingTaskDecorator) {
        // defaultExecutor 是 Spring 自动创建的虚拟线程 Executor
        return new DecoratingExecutor(defaultExecutor, contextPropagatingTaskDecorator);
    }*/
}
