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
package com.lwohvye.config.thread;

import org.springframework.stereotype.Component;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 自定义线程名称
 *
 * @author Zheng Jie
 * @date 2019年10月31日17:49:55
 */
@Component
public class TheadFactoryName implements ThreadFactory {

    private static final AtomicInteger POOL_NUMBER = new AtomicInteger(1);
    private final ThreadGroup group;
    private final AtomicInteger threadNumber = new AtomicInteger(1);
    private final String namePrefix;

    public TheadFactoryName() {
        this("-n_n-pool");
    }

    /**
     * SecurityManager相关参考：https://www.lwohvye.com/2021/11/23/java%e5%ae%89%e5%85%a8%ef%bc%9asecuritymanager%e4%b8%8eaccesscontroller/
     *
     * @param name /
     * @date 2021/11/23 3:32 下午
     */
    private TheadFactoryName(String name) {
        // 一般情况下，安全管理器是默认没有被安装的。
        // System类为用户操作安全管理器提供了两个方法。
        // public static SecurityManager getSecurityManager() 该方法用于获得当前安装的安全管理器引用，若未安装，返回null。
        // public static void setSecurityManager(final SecurityManager s) 该方法用于将指定的安全管理器的实例设置为系统的安全管理器。
        // 所以针对下面的过期，可直接移除
/*        SecurityManager s = System.getSecurityManager();
          group = (s != null) ? s.getThreadGroup() :
                  Thread.currentThread().getThreadGroup();*/
        group = Thread.currentThread().getThreadGroup();
        //此时namePrefix就是 name + 第几个用这个工厂创建线程池的
        this.namePrefix = name + POOL_NUMBER.getAndIncrement();
    }

    @Override
    public Thread newThread(Runnable r) {
        //此时线程的名字 就是 namePrefix + -thread- + 这个线程池中第几个执行的线程
        Thread t = new Thread(group, r, namePrefix + "-thread-" + threadNumber.getAndIncrement(), 0);
        // 所谓后台(daemon)线程，是指在程序运行的时候在后台提供一种通用服务的线程，并且这个线程并不属于程序中不可或缺的部分。
        // 必须在线程启动之前调用setDaemon()方法，才能把它设置为后台线程。注意：后台进程在不执行finally子句的情况下就会终止其run()方法。
        if (t.isDaemon())
            t.setDaemon(false);
        if (t.getPriority() != Thread.NORM_PRIORITY)
            t.setPriority(Thread.NORM_PRIORITY);
        return t;
    }
}
