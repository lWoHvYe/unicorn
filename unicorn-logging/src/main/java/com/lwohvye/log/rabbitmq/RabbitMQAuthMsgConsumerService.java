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
package com.lwohvye.log.rabbitmq;

import com.lwohvye.core.exception.UtilsException;
import com.lwohvye.core.utils.MailAdapter;
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
import java.util.Map;

@Component
@Slf4j
// 监听日志Save相关队列的消息。可以取配置文件中属性
@RabbitListener(queues = "${auth.log.queue-name:auth.log.queue}")
public class RabbitMQAuthMsgConsumerService extends YRabbitAbstractConsumer {

    @Autowired
    private MultiLogService multiLogService;

    private final MethodHandles.Lookup lookup = MethodHandles.lookup();

    @Autowired
    public void setRedissonClient(RedissonClient redissonClient) {
        super.redissonClient = redissonClient;
    }

    // 消费者本身是顺序消费的，且可以避免线程安全问题，有考虑过引入异步，但就无法保证顺序性了，还要解决线程安全问题。等有合适的场景了再做尝试
    @RabbitHandler
    public void handle(String amqpMsgEntityStr) {
        baseConsumer(amqpMsgEntityStr, "authSave", null, msgEntity -> {
            var extraData = msgEntity.getExtraData();
            if (StringUtils.hasText(extraData)) {
                try {
                    var mt = MethodType.methodType(void.class, String.class);
                    var methodHandle = lookup.findVirtual(multiLogService.getClass(), extraData, mt);
                    methodHandle.invoke(multiLogService, msgEntity.getMsgData());
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

