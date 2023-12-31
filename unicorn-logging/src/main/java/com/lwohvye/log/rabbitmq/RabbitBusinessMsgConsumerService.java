/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.log.rabbitmq;

import com.lwohvye.core.exception.UtilsException;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.core.utils.rabbitmq.YRabbitAbstractConsumer;
import com.lwohvye.log.service.local.MultiLogService;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

@Slf4j
@Component
public class RabbitBusinessMsgConsumerService extends YRabbitAbstractConsumer {

    @Autowired
    private MultiLogService multiLogService;

    private final MethodHandles.Lookup lookup = MethodHandles.lookup();

    @Autowired
    public void setRedissonClient(RedissonClient redissonClient) {
        super.redissonClient = redissonClient;
    }

    /**
     * 用来消费业务日志，格式比较统一，这里配合新引入的日志框架使用
     */
    @RabbitHandler
    @RabbitListener(queues = "${business.log.queue-name:business.log.queue}")
    public void handle(String messageStr) {
        baseConsumer(messageStr, null, null, msgEntity -> {
            var extraData = msgEntity.getExtraData();
            if (StringUtils.hasText(extraData)) {
                try {
                    var mt = MethodType.methodType(void.class, String.class, String.class, String.class);
                    var methodHandle = lookup.findVirtual(multiLogService.getClass(), extraData, mt);
                    methodHandle.invoke(multiLogService, msgEntity.getMsgType(), msgEntity.getMsgData(), "Typical Business Operate");
                } catch (Throwable e) {
                    throw new UtilsException(e.getMessage());
                }
            }
            return null;
        }, errMsg -> log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", errMsg, messageStr));
    }

    @Override
    public void baseBeforeConsumer(AmqpMsgEntity msgEntity) {

    }

    @Override
    public void baseBeforeMessageConsumer(AmqpMsgEntity msgEntity) {

    }
}
