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

import java.util.concurrent.Executor;
import org.springframework.core.task.TaskDecorator;
import org.springframework.scheduling.concurrent.ConcurrentTaskExecutor;

/**
 * 装饰 Executor，自动对提交的 Runnable 应用 TaskDecorator
 */
public class DecoratingExecutor extends ConcurrentTaskExecutor {

    public DecoratingExecutor(Executor delegate, TaskDecorator taskDecorator) {
        super(runnable -> delegate.execute(taskDecorator.decorate(runnable)));
    }
}
