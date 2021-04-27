package com.lwohvye.modules.kafka.entity;

import lombok.Getter;

import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

/**
 * @description 自定义规则，队列会根据时间排序。时间短的在前
 * @author Hongyan Wang
 * @date 2021/4/24 20:50
 */
@Getter
public class MyDelayQueue implements Delayed {

    private DelayMessage delayMessage;

    public MyDelayQueue(DelayMessage delayMessage) {
        this.delayMessage = delayMessage;
    }

    @Override
    public long getDelay(TimeUnit unit) {
        return unit.convert(this.delayMessage.getTime(), this.delayMessage.getUnit());
    }

    /**
     * @description 定义比较规则
     * @author Hongyan Wang
     * @date 2021/4/24 20:53
     * @param o
     * @return int
     */
    @Override
    public int compareTo(Delayed o) {
        MyDelayQueue o1 = (MyDelayQueue) o;
        return (int) (this.getDelay(this.delayMessage.getUnit()) - o1.getDelay(o1.getDelayMessage().getUnit()));
    }
}
