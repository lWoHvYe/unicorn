package com.lwohvye.modules.rabbitmq.service;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.modules.kafka.entity.DelayMessage;
import com.lwohvye.modules.kafka.service.KafkaProducerService;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@RabbitListener(queues = "data.sync")
public class RabbitMQConsumerService {

    @Autowired
    private KafkaProducerService kafkaProducerService;

    @RabbitHandler
    public void handle(String amqpMsgEntityStr) {
        var amqpMsgEntity = JSONObject.parseObject(amqpMsgEntityStr, AmqpMsgEntity.class);
        var msgType = amqpMsgEntity.getMsgType();
        var msgData = amqpMsgEntity.getMsgData();
        try {
            if (StrUtil.isBlank(msgData))
                return;
            var delayMessage = JSONObject.parseObject(msgData, DelayMessage.class);
            kafkaProducerService.sendCallbackMessage(delayMessage.getActualTopic(), JSON.toJSONString(delayMessage));
        } finally {
            log.info(amqpMsgEntityStr);
            // TODO: 2021/4/27 处理完成，根据结果记录相关表。并邮件通知
        }
    }

}

