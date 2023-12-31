/*
 *    Copyright (c) 2023-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.utils.rabbitmq;

import cn.hutool.core.util.IdUtil;
import com.lwohvye.core.utils.json.JsonUtils;
import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.amqp.core.MessageBuilder;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.util.Assert;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

public abstract class XRabbitAbstractProducer {

    protected AmqpTemplate amqpTemplate;

    /**
     * 通用发送普通消息
     *
     * @param exchangeName  /
     * @param routeKey      /
     * @param amqpMsgEntity /
     * @date 2022/3/19 1:37 PM
     */
    public void sendMsg(String exchangeName, String routeKey, AmqpMsgEntity amqpMsgEntity) {
        amqpTemplate.convertAndSend(exchangeName, routeKey, JsonUtils.toJSONString(amqpMsgEntity));
    }

    public void sendMsgEntity(String exchangeName, String routeKey, AmqpMsgEntity amqpMsgEntity) {
        amqpTemplate.send(exchangeName, routeKey,
                MessageBuilder.withBody(JsonUtils.toJSONString(amqpMsgEntity).getBytes())
                        .setContentType(MessageProperties.CONTENT_TYPE_JSON)
                        .setContentEncoding("utf-8")
                        .setMessageId(String.valueOf(IdUtil.getSnowflakeNextId()))
                        .build()
        );
        /*
        当把@RabbitListener放在类上时，这种发的要这么接，
        @RabbitHandler
        public void handleMsg(byte[] bytes) {
           // 这里输出的就是上面body中的内容
            System.out.println(new String(bytes));
        }
        当把@RabbitListener放在方法上时，可以这样来接，一般应该是不怎么把这个放类上的吧，还是放方法上比较好
        @RabbitHandler
        public void handleMsg(Message message) {
            var messageId = message.getMessageProperties().getMessageId();
            // 通过messageId判断是否重复消费
            var noConsumer = redissonClient.getMapCache("ConsumerMsgId").fastPutIfAbsent(messageId, "", 5L, TimeUnit.MINUTES);
            if (noConsumer) {
                var msgBody = new String(message.getBody());
                log.warn("消费消息 {}", messageId);
                handle(msgBody);
            }
        }
        */
    }

    /**
     * 通用发送延迟消息
     *
     * @param exchangeName /
     * @param routeKey     /
     * @param commonEntity /
     * @date 2022/3/19 1:38 PM
     */
    public void sendDelayMsg(String exchangeName, String routeKey, AmqpMsgEntity commonEntity) {
        amqpTemplate.convertAndSend(exchangeName, routeKey, JsonUtils.toJSONString(commonEntity),
                message -> {
                    var expire = commonEntity.getExpire();
                    var timeUnit = commonEntity.getTimeUnit();
                    if (Objects.nonNull(expire) && Objects.nonNull(timeUnit)) {
                        Long expireMill = TimeUnit.MILLISECONDS.convert(expire, timeUnit);
                        //通过给消息设置x-delay头来设置消息从交换机发送到队列的延迟时间；
                        message.getMessageProperties().setHeader(MessageProperties.X_DELAY, expireMill);
                    }
                    return message;
                });
    }

    public void sendDelayMsgEntity(String exchangeName, String routeKey, AmqpMsgEntity commonEntity) {
        var expire = commonEntity.getExpire();
        var timeUnit = commonEntity.getTimeUnit();
        Assert.notNull(expire, "请设置消息延迟时间");
        Assert.notNull(timeUnit, "请设置时间单位");
        var message = MessageBuilder.withBody(JsonUtils.toJSONString(commonEntity).getBytes())
                .setContentType(MessageProperties.CONTENT_TYPE_JSON)
                .setContentEncoding("utf-8")
                //通过给消息设置x-delay头来设置消息从交换机发送到队列的延迟时间；
                .setHeader(MessageProperties.X_DELAY, TimeUnit.MILLISECONDS.convert(expire, timeUnit))
                .setMessageId(String.valueOf(IdUtil.getSnowflakeNextId()))
                .build();
        amqpTemplate.send(exchangeName, routeKey, message);
    }
}
