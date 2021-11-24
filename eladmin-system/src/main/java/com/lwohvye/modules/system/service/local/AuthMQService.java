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
import cn.hutool.core.util.StrUtil;
import com.lwohvye.domain.Log;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import com.lwohvye.modules.rabbitmq.service.RabbitMQProducerService;
import com.lwohvye.modules.security.service.UserCacheClean;
import com.lwohvye.modules.system.service.IUserService;
import com.lwohvye.repository.LogRepository;
import com.lwohvye.utils.JsonUtils;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

    public void saveAuthorizeLog(String record) {
        var log = new Log().setDescription("记录用户登录信息").setLogType("Auth").setParams(record);
        logRepository.save(log);
    }
    //    ----------------------登录失败-----------------------------

    @Autowired
    private RedisUtils redisUtils;

    @Autowired
    private IUserService userService;

    @Autowired
    private UserCacheClean userCacheClean;

    @Autowired
    private RabbitMQProducerService rabbitMQProducerService;

    /**
     * @param record
     * @description 消费登录验证不通过的消息
     * @date 2021/10/13 10:22 下午
     */
    public void solveAuthFailed(String record) {
        if (StrUtil.isBlank(record))
            return;
        var infoMap = JsonUtils.toMap(record);
        var ip = JsonUtils.getString(infoMap, "ip");
        var username = JsonUtils.getString(infoMap, "username");
        //          使用 用户名 + ip 作为key
        String authFailedKey = username + "||authFailed||" + ip;
        var countKey = "failed-count";
        var byKey = redisUtils.hGet(authFailedKey, countKey);
        var failCount = ObjectUtil.isNotEmpty(byKey) ? (Integer) byKey : 0;
        log.info(" {} fail-count is {} ", authFailedKey, failCount);
        if (failCount < 5) {
            ++failCount;
            if (ObjectUtil.equal(failCount, 1)) {
//                        新建时设置过期时间5分钟
                redisUtils.hPut(authFailedKey, countKey, failCount, 5 * 60L);
            } else {
//                        更新时只更新值。过期时间不做改动
                redisUtils.hPut(authFailedKey, countKey, failCount);
            }
        } else {
            var lockedIp = JsonUtils.getString(infoMap, "lockedIp");
            // 限制Ip登录 15分钟
            redisUtils.set(lockedIp, authFailedKey, 15L, TimeUnit.MINUTES);

//                  修改用户状态为锁定
            userService.updateEnabled(username, false);
//                  删除缓存中的用户信息
            userCacheClean.cleanUserCache(username);
//                  超过5次锁定一小时。创建延迟解锁消息
            var wait4Unlock = new AmqpMsgEntity().setMsgType("auth").setMsgData(username).setExtraData("unlockUser")
                    .setExpire(1L).setTimeUnit(TimeUnit.HOURS);
//                    延时消息发给RabbitMQ
            rabbitMQProducerService.sendDelayMsg(wait4Unlock);
        }
    }


    public void unlockUser(String record) {
        if (StrUtil.isBlank(record))
            return;
        userService.updateEnabled(record, true);
//              删除缓存中的用户信息
        userCacheClean.cleanUserCache(record);
    }

}
