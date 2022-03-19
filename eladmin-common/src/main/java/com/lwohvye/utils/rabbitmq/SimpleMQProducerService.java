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

import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.utils.json.JsonUtils;
import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

// @Component，在这里做通一的定义，子类通过继承拓展功能。这里就不要只用Component声明为bean了，不然当通过类型获取SimpleMQProducerService时，就会获取到多个
@Component
@ConditionalOnMissingBean(SimpleMQProducerService.class) // 理论上可以这样，当子类未定义时，再注册这个
public class SimpleMQProducerService {

    @Autowired
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
                    if (ObjectUtil.isNotEmpty(expire) && ObjectUtil.isNotEmpty(timeUnit)) {
                        Long expireMill = TimeUnit.MILLISECONDS.convert(expire, timeUnit);
                        //通过给消息设置x-delay头来设置消息从交换机发送到队列的延迟时间；
                        message.getMessageProperties().setHeader("x-delay", expireMill);
                    }
                    return message;
                });
    }
}
