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

import com.lwohvye.core.exception.UtilsException;
import com.lwohvye.core.utils.MailAdapter;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.core.utils.rabbitmq.YRabbitAbstractConsumer;
import com.lwohvye.sys.modules.rabbitmq.config.RabbitMQConfig;
import com.lwohvye.sys.modules.system.service.local.AuthMQService;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.Map;

@Component
@Slf4j
// 监听延迟插件相关队列的消息
@RabbitListener(queues = RabbitMQConfig.DATA_COMMON_DELAY_QUEUE)
// 支持多种配置方式 property placeholders and SpEL  https://docs.spring.io/spring-amqp/docs/current/reference/html/#choose-container
//@RabbitListener(queues = "#{'${property.with.comma.delimited.queue.names}'.split(',')}" )
// 还可调用静态和非静态方法
// SpEL https://www.lwohvye.com/2021/06/11/spring-%e8%a1%a8%e8%be%be%e5%bc%8f%e8%af%ad%e8%a8%80-spel/
//@RabbitListener(queues = "#{T(全类名).静态方法名}")
//@RabbitListener(queues = "#{beanName.方法名}")

// 当将@RabbitListener注解放在类上时，一些情况下会报 org.springframework.amqp.AmqpException: No method found for class [B
// 相关原因参见：https://jira.spring.io/browse/AMQP-573
// 可使用将@RabbitListener放到方法上的方式。因为类型推断只适用于方法级别的@RabbitListener
// 使用@Payload注解可以获取消息中的body信息，使用@Headers注解可以获取消息中的headers信息，也可以使用@Header获取单个header属性

// @RabbitListener 可以标注在类上面，需配合 @RabbitHandler 注解一起使用
// @RabbitListener 标注在类上面表示当有收到消息的时候，就交给 @RabbitHandler 的方法处理，具体使用哪个方法处理，根据 MessageConverter 转换后的参数类型
public class RabbitMQDelayMsgConsumerService extends YRabbitAbstractConsumer {

    @Autowired
    private AuthMQService authMQService;

    private final MethodHandles.Lookup lookup = MethodHandles.lookup();

    @Autowired
    public void setRedissonClient(RedissonClient redissonClient) {
        super.redissonClient = redissonClient;
    }

    // 消费者本身是顺序消费的，且可以避免线程安全问题，有考虑过引入异步，但就无法保证顺序性了，还要解决线程安全问题。等有合适的场景了再做尝试
    @RabbitHandler
    public void handle(String amqpMsgEntityStr) {
        baseConsumer(amqpMsgEntityStr, "auth", null, msgEntity -> {
            var extraData = msgEntity.getExtraData();
            if (StringUtils.hasText(extraData)) {
                try {
                    var mt = MethodType.methodType(void.class, new Class[]{String.class});
                    var methodHandle = lookup.findVirtual(authMQService.getClass(), extraData, mt);
                    methodHandle.invoke(authMQService, msgEntity.getMsgData());
                } catch (Throwable e) {
                    throw new UtilsException(e.getMessage());
                }
            }
            return null;
        }, errMsg -> {
            // reConsumeMsg(this::handle, amqpMsgEntityStr);
            var to = "";
            var subject = "Consume Msg Error" + this.getClass().getSimpleName();
            var templateName = "email/noticeEmail.ftl";
            var res = MailAdapter.sendTemplatedMail(to, subject, templateName, Map.of("errMsg", errMsg));
            log.error(" Consume Msg Error, Reason: {} || Msg detail: {} || NoticeRes {} ", errMsg, amqpMsgEntityStr, res);
        });
    }

    @RabbitHandler
    public void handleMsg(byte[] bytes) {
        handle(new String(bytes));
    }

    @Override
    public void baseBeforeConsumer(AmqpMsgEntity msgEntity) {

    }

    @Override
    public void baseBeforeMessageConsumer(AmqpMsgEntity msgEntity) {

    }
}

