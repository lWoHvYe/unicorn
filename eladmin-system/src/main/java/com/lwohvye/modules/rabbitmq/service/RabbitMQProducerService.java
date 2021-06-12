package com.lwohvye.modules.rabbitmq.service;

import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.config.rabbitmq.QueueEnum;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;


@Component
@Slf4j
public class RabbitMQProducerService {

    @Autowired
    private AmqpTemplate amqpTemplate;

    /**
     * @param amqpMsgEntity
     * @description 发送消息
     * @author Hongyan Wang
     * @date 2021/4/27 2:49 下午
     */
    public void sendMsg(AmqpMsgEntity amqpMsgEntity) {
        amqpTemplate.convertAndSend(QueueEnum.QUEUE_DATA_SYNC.getExchange(), QueueEnum.QUEUE_DATA_SYNC.getRouteKey(), JSONObject.toJSONString(amqpMsgEntity));

    }

    /**
     * @param amqpMsgEntity
     * @description 发送延时消息
     * 由于队列的先进先出特性，只有当过期的消息到了队列的顶端（队首），才会被真正的丢弃或者进入死信队列。
     * 所以在考虑使用RabbitMQ来实现延迟任务队列的时候，需要确保业务上每个任务的延迟时间是一致的。
     * 如果遇到不同的任务类型需要不同的延时的话，需要为每一种不同延迟时间的消息建立单独的消息队列。
     * 解决方式是使用延迟插件。可根据需要调整
     *
     * @author Hongyan Wang
     * @date 2021/4/27 2:49 下午
     */
    public void sendTTLMsg(AmqpMsgEntity amqpMsgEntity) {
        //给延迟队列发送消息
        amqpTemplate.convertAndSend(QueueEnum.QUEUE_DATA_SYNC_TTL.getExchange(), QueueEnum.QUEUE_DATA_SYNC_TTL.getRouteKey(), JSONObject.toJSONString(amqpMsgEntity),
                message -> {
//                    将延时转为毫秒值
                    var expire = amqpMsgEntity.getExpire();
                    var timeUnit = amqpMsgEntity.getTimeUnit();
                    if (ObjectUtil.isNotEmpty(expire) && ObjectUtil.isNotEmpty(timeUnit)) {
                        var expireMill = TimeUnit.MILLISECONDS.convert(expire, timeUnit);
                        //给消息设置延迟毫秒值
                        message.getMessageProperties().setExpiration(String.valueOf(expireMill));
                    }
                    return message;
                });
    }

}
