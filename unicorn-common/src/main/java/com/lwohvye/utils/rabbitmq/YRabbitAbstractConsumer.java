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

package com.lwohvye.utils.rabbitmq;

import com.lwohvye.utils.json.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.core.Message;
import org.springframework.util.StringUtils;

import java.util.Objects;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * 事件消费者的模板方法
 *
 * @date 2022/3/25 4:50 PM
 */
@Slf4j
public abstract class YRabbitAbstractConsumer {

    // 使用线程池，做资源隔离。考虑到服务器配置，因为会有多个独立的线程池，这里根据需要调小一些
    static final ThreadPoolExecutor SIMPLE_EXECUTOR = new ThreadPoolExecutor(4, 6, 200, TimeUnit.SECONDS, new ArrayBlockingQueue<>(100));


    // 若用到该属性，子类需通过set注入
    protected RedissonClient redissonClient;

    /**
     * @param msgStr           消息
     * @param allowedMsgType   支持的消息类型
     * @param curOrigin        当前实例标识
     * @param consumerFunction 具体的消费方法
     * @param consumerFailed   消费失败时回调
     * @date 2022/3/25 3:57 PM
     */
    public Object baseConsumer(String msgStr, String allowedMsgType, String curOrigin, Function<AmqpMsgEntity, Object> consumerFunction, Consumer<String> consumerFailed) {
        var amqpMsgEntity = JsonUtils.toJavaObject(msgStr, AmqpMsgEntity.class);
        var msgType = amqpMsgEntity.getMsgType();
        var msgData = amqpMsgEntity.getMsgData();
        var origin = amqpMsgEntity.getOrigin();
        try {
            // 无消息体，不消费
            if (!StringUtils.hasLength(msgData))
                return null;
            // 当限制消息类型时，类型不符则不消费
            if (StringUtils.hasText(allowedMsgType) && !Objects.equals(msgType, allowedMsgType))
                return null;
            // 本实例产生的事件，忽略即可
            if (StringUtils.hasText(curOrigin) && Objects.equals(origin, curOrigin))
                return null;
            baseBeforeConsumer(amqpMsgEntity);
            return consumerFunction.apply(amqpMsgEntity);
        } catch (Exception e) {
            log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", e.getMessage(), msgStr);
            consumerFailed.accept(e.getMessage());
            return null;
        } finally {
            log.info("Consume Msg,Msg type: {}, -+- ,Msg detail: {}", msgType, msgStr);
        }
    }

    // 引入Function、Consumer这些之后，这个抽象方法可以不再使用了
    public abstract void baseBeforeConsumer(AmqpMsgEntity msgEntity);

    /**
     * @param message          消息
     * @param allowedMsgType   支持的消息类型
     * @param curOrigin        当前实例标识
     * @param checkedCache     重复消费校验用key。不传则不做校验
     * @param consumerFunction 具体的消费方法
     * @param consumerFailed   消费失败回调
     * @date 2022/3/25 3:59 PM
     */
    public Object baseMessageConsumer(Message message, String allowedMsgType, String curOrigin, String checkedCache, Function<AmqpMsgEntity, Object> consumerFunction, Consumer<String> consumerFailed) {

        var messageId = message.getMessageProperties().getMessageId();
        var msgBody = new String(message.getBody());
        var amqpMsgEntity = JsonUtils.toJavaObject(msgBody, AmqpMsgEntity.class);
        var msgType = amqpMsgEntity.getMsgType();
        var origin = amqpMsgEntity.getOrigin();

        try {
            // 当限制消息类型时，类型不符则不消费
            if (StringUtils.hasText(allowedMsgType) && !Objects.equals(msgType, allowedMsgType))
                return null;
            // 本实例产生的事件，忽略即可
            if (StringUtils.hasText(curOrigin) && Objects.equals(origin, curOrigin))
                return null;
            // 通过messageId判断是否重复消费，因为事件可能会有广播类的，所以这里的cacheKey需根据情况确定是通用类、服务色彩、单个实例色彩
            var noConsumer = !StringUtils.hasText(checkedCache) || redissonClient.getMapCache(checkedCache + curOrigin).fastPutIfAbsent(messageId, "", 5L, TimeUnit.MINUTES);
            // 已经消费过，则跳过
            if (Boolean.FALSE.equals(noConsumer))
                return null;
            baseBeforeMessageConsumer(amqpMsgEntity);
            return consumerFunction.apply(amqpMsgEntity);
        } catch (Exception e) {
            log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", e.getMessage(), msgBody);
            consumerFailed.accept(e.getMessage());
            return null;
        } finally {
            log.info("Consume Msg,Msg type: {}, -+- ,Msg detail: {}", msgType, msgBody);
        }
    }

    public abstract void baseBeforeMessageConsumer(AmqpMsgEntity msgEntity);

    /**
     * 暂停2s后，重新消费，只会重复一次
     *
     * @param consumer 消费逻辑
     * @param message  消费对象
     * @date 2022/3/27 10:52 AM
     */
    protected void reConsumeMsg(Consumer<Message> consumer, Message message) {
        // 线程池来执行，异步
        SIMPLE_EXECUTOR.execute(() -> {
            // 打个标记，只会重消费一次，不然就无穷无尽了
            var mask = "ReConsumed";
            var header = message.getMessageProperties().getHeader(mask);
            if (Objects.isNull(header)) {
                message.getMessageProperties().setHeader(mask, "Ignored");
                try {
                    // 暂停2s后，再重新消费一次
                    Thread.sleep(1500L);
                    consumer.accept(message);
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
            }
        });
    }

    protected void reConsumeMsg(Consumer<String> consumer, String strMsg) {
        SIMPLE_EXECUTOR.execute(() -> {
            var amqpMsgEntity = JsonUtils.toJavaObject(strMsg, AmqpMsgEntity.class);
            if (!amqpMsgEntity.isConsumed()) {
                amqpMsgEntity.setConsumed(true);
                try {
                    // 暂停2s后，再重新消费一次
                    Thread.sleep(1500L);
                    consumer.accept(JsonUtils.toJSONString(amqpMsgEntity));
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
            }
        });
    }

    @SuppressWarnings("unchecked")
    protected <T> void reConsumeMsg(Consumer<T> consumer, T tMsg) {
        if (tMsg instanceof Message message) {
            reConsumeMsg((Consumer<Message>) consumer, message);
        } else if (tMsg instanceof String strMsg) {
            reConsumeMsg((Consumer<String>) consumer, strMsg);
        } else {
            log.warn("暂不支持类型 {} ，请自行定义扩展", tMsg.getClass().getSimpleName());
        }
    }

}
