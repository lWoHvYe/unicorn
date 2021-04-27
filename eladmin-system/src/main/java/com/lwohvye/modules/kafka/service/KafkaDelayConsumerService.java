package com.lwohvye.modules.kafka.service;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSON;
import com.lwohvye.modules.kafka.entity.DelayMessage;
import com.lwohvye.modules.kafka.entity.MyDelayQueue;
import com.lwohvye.modules.kafka.enums.KafkaConstants;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import java.util.concurrent.DelayQueue;

@Slf4j
@Component
public class KafkaDelayConsumerService {

    @Autowired
    private KafkaProducerService kafkaProducerService;

    // 集合
    private static DelayQueue<MyDelayQueue> delayQueue = new DelayQueue<>();

    /**
     * 监听
     *
     * @param rc
     * @return
     */
    @KafkaListener(id = "consumer-delay", groupId = "felix-group", topics = {KafkaConstants.KAFKA_TOPIC_MESSAGE_DELAY}, containerFactory = "filterContainerFactory")
    public boolean onDelayMessage(ConsumerRecord<?, ?> rc) {
        try {
            if (rc.value() instanceof String json) {
                var delayMessage = JSON.parseObject(json, DelayMessage.class);
                if (!isDelay(delayMessage)) {
                    // 如果接收到消息时，消息已经可以发送了，直接发送到实际的队列
                    sendActualTopic(delayMessage);
                } else {
                    // 存储
                    localStorage(delayMessage);
                }
            }
        } catch (Throwable e) {
            log.error("consumer kafka delay message[{}] error!", rc, e);
            throw e;
        }
        return true;
    }

    /**
     * 立即执行
     *
     * @param delayMessage
     * @return
     */
    private boolean isDelay(DelayMessage delayMessage) {
        return delayMessage.getTime().compareTo(0L) != 0;
    }

    /**
     * 发送消息
     *
     * @param delayMessage
     */
    private void sendActualTopic(DelayMessage delayMessage) {
        kafkaProducerService.sendCallbackMessage(delayMessage.getActualTopic(), JSON.toJSONString(delayMessage));
    }

    /**
     * 添加集合
     *
     * @param delayMessage
     */
    @SneakyThrows
    private void localStorage(DelayMessage delayMessage) {
        delayQueue.add(new MyDelayQueue(delayMessage));
    }

    /**
     * 加载监听。
     */
    // TODO: 2021/4/27 这系列方法不完善。且队列中消息没有消费。后续排除原因
    public void handleDelayQueue() throws InterruptedException {
        while (CollUtil.isNotEmpty(delayQueue)) {
            // 取出队列
            var take = delayQueue.take();
//                未获取到，则结束此轮。
            if (ObjectUtil.isEmpty(take))
                return;
            // 将队列发送到队列中
            var delayMessage = take.getDelayMessage();
            sendActualTopic(delayMessage);
        }
    }
}
