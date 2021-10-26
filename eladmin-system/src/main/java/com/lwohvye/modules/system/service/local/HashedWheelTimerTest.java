package com.lwohvye.modules.system.service.local;

import io.netty.util.HashedWheelTimer;
import io.netty.util.Timeout;
import io.netty.util.TimerTask;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.TimeUnit;

/**
 * @author Hongyan Wang
 * @description 时间轮
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
