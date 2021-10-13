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
package com.lwohvye.modules.system.service.local;

import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.config.redis.AuthRedisUtils;
import com.lwohvye.config.redis.AuthSlaveRedisUtils;
import com.lwohvye.domain.Log;
import com.lwohvye.modules.kafka.entity.DelayMessage;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import com.lwohvye.modules.rabbitmq.service.RabbitMQProducerService;
import com.lwohvye.modules.security.service.UserCacheClean;
import com.lwohvye.modules.system.service.UserService;
import com.lwohvye.repository.LogRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * @author Hongyan Wang
 * @description 这里用了consumerAwareErrorHandler在另一个类KafkaConsumerService里。不知什么原因就造成了循环依赖
 * 但在idea中运行时没有问题，打jar部署就有问题。
 * 已知的一个原因是依赖未及时更新。但这不是导致该问题的原因
 * 因为starter模块依赖system模块，但system更新后，starter打包时用的还是原来的。但用idea运行时就是新的。这是一个差异
 * @date 2021年04月21日 21:29
 */
@Slf4j
@Component
public class AuthMQService {
    //    -------------------记录鉴权信息-----------------------------
    @Autowired
    private LogRepository logRepository;

    @KafkaListener(id = "authLogConsumer", groupId = "felix-group", topics = "auth-log", errorHandler = "consumerAwareErrorHandler")
    public void saveAuthorizeLog(List<ConsumerRecord<?, ?>> records) {
        for (ConsumerRecord<?, ?> record : records) {
            var log = new Log().setDescription("记录用户登录信息").setLogType("Auth").setParams(record.toString());
            logRepository.save(log);
        }
    }
    //    ----------------------登录失败-----------------------------

    @Autowired
    private AuthRedisUtils authRedisUtils;

    @Autowired
    private AuthSlaveRedisUtils authSlaveRedisUtils;

    @Autowired
    private UserService userService;

    @Autowired
    private UserCacheClean userCacheClean;

    @Autowired
    private RabbitMQProducerService rabbitMQProducerService;

    /**
     * @param records
     * @description 消费登录验证不通过的消息
     * @date 2021/10/13 10:22 下午
     */
    @KafkaListener(id = "authFailedConsumer", groupId = "felix-group", topics = "auth-failed", errorHandler = "consumerAwareErrorHandler")
    public void solveAuthFailed(List<ConsumerRecord<?, ?>> records) {
        for (ConsumerRecord<?, ?> record : records) {
            var value = record.value();
            if (value instanceof String infoStr) {
                var infoJson = JSONObject.parseObject(infoStr);
                var ip = infoJson.getString("ip");
                var username = infoJson.getString("username");
                //          使用 用户名 + ip 作为key
                String authFailedKey = username + "||authFailed||" + ip;
                var countKey = "failed-count";
                var byKey = authSlaveRedisUtils.hGet(authFailedKey, countKey);
                var failCount = ObjectUtil.isNotEmpty(byKey) ? (Integer) byKey : 0;
                log.info("fail-count" + failCount);
                // TODO: 2021/7/5 需要加锁。但消息是顺序消费的，也许不加也行，但消费者可以有多个，最好加上
                if (failCount < 5) {
                    failCount += 1;
                    if (ObjectUtil.equal(failCount, 1)) {
//                        新建时设置过期时间5分钟
                        authRedisUtils.hPut(authFailedKey, countKey, failCount, 5 * 60L);
                    } else {
//                        更新时只更新值。过期时间不做改动
                        authRedisUtils.hPut(authFailedKey, countKey, failCount);
                    }
                } else {
//                  修改状态为锁定
                    userService.updateEnabled(username, false);
//                  删除缓存中的用户信息
                    userCacheClean.cleanUserCache(username);
//                  超过5次锁定一小时
                    var delayMessage = new DelayMessage();
                    delayMessage.setActualTopic("unlock-user").setContext(username);
                    var amqpMsgEntity = new AmqpMsgEntity().setMsgType("auth_locked").setMsgData(JSONObject.toJSONString(delayMessage))
                            .setExpire(1L).setTimeUnit(TimeUnit.HOURS);
//                    延时消息发给RabbitMQ
                    rabbitMQProducerService.sendDelayMsg(amqpMsgEntity);
//                超过5次锁定一小时
                }
            }
        }
    }


    @KafkaListener(id = "unlockUserConsumer", groupId = "felix-group", topics = "unlock-user", errorHandler = "consumerAwareErrorHandler")
    public void unlockUser(List<ConsumerRecord<?, ?>> records) {
        for (ConsumerRecord<?, ?> record : records) {
            var value = record.value();
            if (value instanceof String delayMessageStr) {
                var delayMessage = JSONObject.parseObject(delayMessageStr, DelayMessage.class);
                var username = delayMessage.getContext();
                userService.updateEnabled(username, true);
//              删除缓存中的用户信息
                userCacheClean.cleanUserCache(username);
            }
        }
    }

}
