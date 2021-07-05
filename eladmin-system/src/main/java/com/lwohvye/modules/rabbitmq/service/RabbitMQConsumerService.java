/*
 *  Copyright 2020-2022 lWoHvYe
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

