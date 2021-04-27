package com.lwohvye.modules.system.service.local;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.config.kafka.KafkaProducerUtils;
import com.lwohvye.config.rabbitmq.RabbitMQProducer;
import com.lwohvye.config.redis.AuthRedisUtils;
import com.lwohvye.config.redis.AuthSlaveRedisUtils;
import com.lwohvye.domain.Log;
import com.lwohvye.modules.system.service.UserService;
import com.lwohvye.repository.LogRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author Hongyan Wang
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
    private KafkaProducerUtils kafkaProducerUtils;

    @Autowired
    private RabbitMQProducer rabbitMQProducer;

    @KafkaListener(id = "authFailedConsumer", groupId = "felix-group", topics = "auth-failed", errorHandler = "consumerAwareErrorHandler")
    public void solveAuthFailed(List<ConsumerRecord<?, ?>> records) {
        for (ConsumerRecord<?, ?> record : records) {
            var value = record.value();
            if (value instanceof String infoStr) {
                var infoJson = JSONObject.parseObject(infoStr);
                var ip = infoJson.getString("ip");
                var lockUserKey = infoJson.getString("lockUserKey");
                var username = infoJson.getString("username");
                //          使用 用户名 + ip 作为key
                String authFailedKey = username + "||authFailed||" + ip;
                var countKey = "failed-count";
                var byKey = authSlaveRedisUtils.hget(authFailedKey, countKey);
                var failCount = ObjectUtil.isNotEmpty(byKey) ? (Integer) byKey : 0;
                log.info("fail-count" + failCount);
                if (failCount < 5) {
                    failCount += 1;
                    if (ObjectUtil.equal(failCount, 1)) {
//                        新建时设置过期时间5分钟
                        authRedisUtils.hset(authFailedKey, countKey, failCount, 5 * 60L);
                    } else {
//                        更新时只更新值。过期时间不做改动
                        authRedisUtils.hset(authFailedKey, countKey, failCount);
                    }
                } else {
//                  修改状态为锁定
                    userService.updateEnabled(username, false);
//                超过5次锁定一小时
                    // TODO: 2021/4/22 延时队列
                    // TODO: 2021/4/27 延时通过RabbitMQ实现。
//                    rabbitMQProducer
//                    kafkaProducerUtils.sendCallbackMessage("unlock-user", username);
                }
            }
        }
    }


    @KafkaListener(id = "unlockUserConsumer", groupId = "felix-group", topics = "unlock-user", errorHandler = "consumerAwareErrorHandler")
    public void unlockUser(List<ConsumerRecord<?, ?>> records) {
        for (ConsumerRecord<?, ?> record : records) {
            var value = record.value();
            if (value instanceof String username)
                userService.updateEnabled(username, true);
        }
    }

}
