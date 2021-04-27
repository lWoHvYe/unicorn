package com.lwohvye.config.rabbitmq;

import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;


@Component
@Slf4j
public class RabbitMQProducer {

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
