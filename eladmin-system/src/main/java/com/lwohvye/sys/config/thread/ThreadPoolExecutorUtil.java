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
package com.lwohvye.sys.config.thread;

import com.lwohvye.utils.SpringContextHolder;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * 用于获取自定义线程池。当前用于定时任务的执行，参数可以根据需要调整。虽然用了异步线程池的参数，但跟其是隔离的
 *
 * @author Zheng Jie
 * @date 2019年10月31日18:16:47
 */
public class ThreadPoolExecutorUtil {

    public static ThreadPoolExecutor getPoll() {
        AsyncTaskProperties properties = SpringContextHolder.getBean(AsyncTaskProperties.class);
        // 这里只有异步线程池的一半
        return new ThreadPoolExecutor(
                properties.getCorePoolSize() >> 1,
                properties.getMaxPoolSize() >> 1,
                properties.getKeepAliveSeconds(),
                TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(properties.getQueueCapacity()),
                new TheadFactoryName()
        );
    }
}
