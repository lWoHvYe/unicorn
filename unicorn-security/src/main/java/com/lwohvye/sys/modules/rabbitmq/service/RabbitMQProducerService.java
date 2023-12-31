/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.sys.modules.rabbitmq.service;

import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.core.utils.json.JsonUtils;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.beans.rabbitmq.ExtensionProducerService;
import com.lwohvye.sys.modules.rabbitmq.config.RabbitMQConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

// 发消息时指定了 交换机和路由键。 所以可以把发消息 和 队列的绑定分开，发消息方定义交换机、消费方定义队列 及队列与交换机、路由键的绑定。提高灵活性。
@Component
@Slf4j
public class RabbitMQProducerService extends ExtensionProducerService {

    /**
     * 发送消息
     *
     * @param amqpMsgEntity
     * @date 2021/4/27 2:49 下午
     */
    public void sendMsg(AmqpMsgEntity amqpMsgEntity) {
        sendMsg(RabbitMQConfig.DIRECT_SYNC_EXCHANGE, RabbitMQConfig.DATA_SYNC_ROUTE_KEY, amqpMsgEntity);
    }

    /**
     * 发送延时消息
     * 由于队列的先进先出特性，只有当过期的消息到了队列的顶端（队首），才会被真正的丢弃或者进入死信队列。
     * 所以在考虑使用RabbitMQ来实现延迟任务队列的时候，需要确保业务上每个任务的延迟时间是一致的。
     * 如果遇到不同的任务类型需要不同的延时的话，需要为每一种不同延迟时间的消息建立单独的消息队列。
     * 解决方式是使用延迟插件。可根据需要调整
     *
     * @param amqpMsgEntity
     * @date 2021/4/27 2:49 下午
     */
    public void sendTTLMsg(AmqpMsgEntity amqpMsgEntity) {
        //给延迟队列发送消息
        amqpTemplate.convertAndSend(RabbitMQConfig.DIRECT_SYNC_TTL_EXCHANGE, RabbitMQConfig.DATA_SYNC_TTL_ROUTE_KEY, JsonUtils.toJSONString(amqpMsgEntity),
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

    /**
     * 通过延迟插件实现延迟消息
     *
     * @param commonEntity
     * @date 2021/7/26 1:17 下午
     */
    public void sendDelayMsg(AmqpMsgEntity commonEntity) {
        sendDelayMsg(RabbitMQConfig.DIRECT_SYNC_DELAY_EXCHANGE, RabbitMQConfig.DATA_COMMON_DELAY_ROUTE_KEY, commonEntity);
    }

    /**
     * 延迟消息，topic模式
     *
     * @param routeKey     路由键
     * @param commonEntity 消息体
     * @date 2021/9/30 1:38 下午
     */
    public void sendSyncDelayMsg(String routeKey, AmqpMsgEntity commonEntity) {
        // 延时500ms
        commonEntity.setExpire(500L).setTimeUnit(TimeUnit.MILLISECONDS);
        sendDelayMsg(RabbitMQConfig.TOPIC_SYNC_DELAY_EXCHANGE, routeKey, commonEntity);
    }

    public void sendSyncDelayMsgEntity(String routeKey, AmqpMsgEntity commonEntity) {
        // 延时500ms
        commonEntity.setExpire(500L).setTimeUnit(TimeUnit.MILLISECONDS);
        sendDelayMsgEntity(RabbitMQConfig.TOPIC_SYNC_DELAY_EXCHANGE, routeKey, commonEntity);
    }
}
