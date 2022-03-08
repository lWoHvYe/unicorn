/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.modules.rabbitmq.service;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.core.util.StrUtil;
import com.lwohvye.config.rabbitmq.RabbitMqConfig;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import com.lwohvye.modules.security.service.UserCacheClean;
import com.lwohvye.utils.json.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
@Slf4j
public class RabbitMQSPMsgConsumerService {

    private UserCacheClean userCacheClean;

    @Autowired // Spring循环依赖问题，可以通过将构造注入改为setter注入的方式解决（三个Map）。也可以使用@Lazy注解。还有些别的解决方式
    // 这里只是做一个记录。UserCacheClean并未构成循环依赖
    public void setUserCacheClean(UserCacheClean userCacheClean) {
        this.userCacheClean = userCacheClean;
    }

    @RabbitHandler
    @RabbitListener(queues = "#{rabbitMQSPMsgConsumerService.getSPMsgQueue()}") // 可以通过SpEL从别处获取监听的队列名
    public void spMsgConsumer(String amqpMsgEntityStr) {
        var amqpMsgEntity = JsonUtils.toJavaObject(amqpMsgEntityStr, AmqpMsgEntity.class);
        var msgType = amqpMsgEntity.getMsgType();
        var msgData = amqpMsgEntity.getMsgData();
        var origin = amqpMsgEntity.getOrigin();
        try {
            if (Objects.equals(origin, RabbitMqConfig.ORIGIN))
                return; // 本实例产生的事件，忽略即可

            if (ObjectUtil.equals(msgType, "sp")) {
                var extraData = amqpMsgEntity.getExtraData();
                if (StrUtil.isNotBlank(extraData))
                    ReflectUtil.invoke(userCacheClean, extraData, msgData, false);
            }
        } catch (Exception e) {
            log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", e.getMessage(), amqpMsgEntityStr);
        } finally {
            log.info("Consume Msg,Msg type: {}, -+- ,Msg detail: {}", msgType, amqpMsgEntityStr);
        }
    }

    public String getSPMsgQueue() {
        return RabbitMqConfig.SP_SYNC_DELAY_QUEUE;
    }
}
