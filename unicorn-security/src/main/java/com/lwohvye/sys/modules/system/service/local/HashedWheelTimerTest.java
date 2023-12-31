/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.sys.modules.system.service.local;

import io.netty.util.HashedWheelTimer;
import io.netty.util.Timeout;
import io.netty.util.TimerTask;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.TimeUnit;

/**
 * 时间轮
 *
 * @author Hongyan Wang
 * @date 2021年10月26日 12:33
 */
@Slf4j
public class HashedWheelTimerTest {
    // 延时执行的任务
    static class MyTimerTask implements TimerTask {

        boolean flag;

        public MyTimerTask(boolean flag) {
            this.flag = flag;
        }

        @Override
        public void run(Timeout timeout) throws Exception {
            log.error("去工作的快乐背影。。。。");
            this.flag = false;
        }
    }

    public static void main(String[] args) {
        var timerTask = new MyTimerTask(true);
        // 时间轮
        var wheelTimer = new HashedWheelTimer();
        wheelTimer.newTimeout(timerTask, 18, TimeUnit.SECONDS);

        var i = 1;
        while (timerTask.flag) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            log.warn("{} 秒已经过去了", i++);
        }
    }
}
