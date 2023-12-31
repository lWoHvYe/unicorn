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

package com.lwohvye.sys.modules.rabbitmq.service;

import com.lwohvye.beans.config.LocalPropertyConfig;
import com.lwohvye.core.exception.UtilsException;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.core.utils.rabbitmq.YRabbitAbstractConsumer;
import com.lwohvye.sys.modules.security.service.UserLocalCache;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

@Component
@Slf4j
public class RabbitMQSPMsgConsumerService extends YRabbitAbstractConsumer {

    private UserLocalCache userLocalCache;

    private final MethodHandles.Lookup lookup = MethodHandles.lookup();

    @Autowired // Spring循环依赖问题，可以通过将构造注入改为setter注入的方式解决（三个Map）。也可以使用@Lazy注解。还有些别的解决方式
    // 这里只是做一个记录。UserCacheClean并未构成循环依赖
    public void setUserCacheClean(UserLocalCache userLocalCache) {
        this.userLocalCache = userLocalCache;
    }

    @Autowired
    public void setRedissonClient(RedissonClient redissonClient) {
        super.redissonClient = redissonClient;
    }

    @RabbitHandler
    @RabbitListener(queues = "#{localPropertyConfig.SP_SYNC_DELAY_QUEUE}") // 可以通过SpEL从别处获取监听的队列名
    public void spMsgConsumer(Message message) {
        var curOrigin = LocalPropertyConfig.ORIGIN;
        var checkedCache = "ConsumerSpMsgId";
        baseMessageConsumer(message, "sp", curOrigin, checkedCache, msgEntity -> {
            var extraData = msgEntity.getExtraData();
            if (StringUtils.hasText(extraData))
                // 这里的逻辑比较简单，首先内部已经做了忽略本实例产生的消息的逻辑。视情况可能还要做：有时需要忽略本集群产生的事件，有时需要向内部传递调用方为MQ消费者从而视情况不进行事件的扩散（虽然一般都是来自消费者的调用不做数据及事件的同步）
                try {
                    var mt = MethodType.methodType(void.class, String.class, Boolean.class);
                    var methodHandle = lookup.findVirtual(userLocalCache.getClass(), extraData, mt);
                    methodHandle.invoke(userLocalCache, msgEntity.getMsgData(), false);
                } catch (Throwable e) {
                    throw new UtilsException(e.getMessage());
                }
            return null;
        }, s -> {
            // 先移除消费过的标志，再主动重新消费一下。考虑了一下，这种cancel还是交给子类，否则要额外传个Consumer进去了
            redissonClient.getMapCache(checkedCache + curOrigin).remove(message.getMessageProperties().getMessageId());
            reConsumeMsg(this::spMsgConsumer, message);
        });
    }

    @Override
    public void baseBeforeConsumer(AmqpMsgEntity msgEntity) {

    }

    @Override
    public void baseBeforeMessageConsumer(AmqpMsgEntity msgEntity) {

    }
}
