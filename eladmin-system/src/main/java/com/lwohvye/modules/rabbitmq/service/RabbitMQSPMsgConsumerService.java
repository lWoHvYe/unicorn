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
import com.lwohvye.config.LocalCoreConfig;
import com.lwohvye.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.modules.security.service.UserLocalCache;
import com.lwohvye.utils.json.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class RabbitMQSPMsgConsumerService {

    private UserLocalCache userLocalCache;
    private RedissonClient redissonClient;

    @Autowired // Spring循环依赖问题，可以通过将构造注入改为setter注入的方式解决（三个Map）。也可以使用@Lazy注解。还有些别的解决方式
    // 这里只是做一个记录。UserCacheClean并未构成循环依赖
    public void setUserCacheClean(UserLocalCache userLocalCache) {
        this.userLocalCache = userLocalCache;
    }

    @RabbitHandler
    @RabbitListener(queues = "#{localCoreConfig.SP_SYNC_DELAY_QUEUE}") // 可以通过SpEL从别处获取监听的队列名
    public void spMsgConsumer(Message message) {
        var messageId = message.getMessageProperties().getMessageId();
        // 通过messageId判断是否重复消费，因为事件可能会有广播类的，所以这里的cacheKey需根据情况确定是通用类、服务色彩、单个实例色彩
        var noConsumer = redissonClient.getMapCache("ConsumerSpMsgId" + LocalCoreConfig.ORIGIN).fastPutIfAbsent(messageId, "", 5L, TimeUnit.MINUTES);
        if (noConsumer) {
            var msgBody = new String(message.getBody());
            var amqpMsgEntity = JsonUtils.toJavaObject(msgBody, AmqpMsgEntity.class);
            var msgType = amqpMsgEntity.getMsgType();
            var msgData = amqpMsgEntity.getMsgData();
            var origin = amqpMsgEntity.getOrigin();
            try {
                if (Objects.equals(origin, LocalCoreConfig.ORIGIN))
                    return; // 本实例产生的事件，忽略即可

                if (ObjectUtil.equals(msgType, "sp")) {
                    var extraData = amqpMsgEntity.getExtraData();
                    if (StrUtil.isNotBlank(extraData))
                        ReflectUtil.invoke(userLocalCache, extraData, msgData, false);
                }
            } catch (Exception e) {
                log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", e.getMessage(), msgBody);
            } finally {
                log.info("Consume Msg,Msg type: {}, -+- ,Msg detail: {}", msgType, msgBody);
            }
        }
    }

}
