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
// 监听延迟插件相关队列的消息
@RabbitListener(queues = "data.common.delay")
// 支持多种配置方式 property placeholders and SpEL  https://docs.spring.io/spring-amqp/docs/current/reference/html/#choose-container
//@RabbitListener(queues = "#{'${property.with.comma.delimited.queue.names}'.split(',')}" )
// 还可调用静态和非静态方法
// SpEL https://www.lwohvye.com/2021/06/11/spring-%e8%a1%a8%e8%be%be%e5%bc%8f%e8%af%ad%e8%a8%80-spel/
//@RabbitListener(queues = "#{T(全类名).静态方法名}")
//@RabbitListener(queues = "#{beanName.方法名}")
public class RabbitMQDelayMsgConsumerService {

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
        } catch (Exception e) {
            log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", e.getMessage(), amqpMsgEntityStr);
        } finally {
            log.info("Consume Msg,Msg type: {}, -+- ,Msg detail: {}", msgType, amqpMsgEntityStr);
            // TODO: 2021/4/27 处理完成，根据结果记录相关表。若处理报错，需邮件通知
        }
    }

}

