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

package com.lwohvye.beans.rabbitmq;

import com.lwohvye.core.utils.rabbitmq.XRabbitAbstractProducer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.stereotype.Component;

/**
 * 在这里做通一的定义，子类通过继承拓展功能。这里就不要只用Component声明为bean了，不然当通过类型获取SimpleMQProducerService时，就会获取到多个
 *
 * @date 2022/3/19 2:37 PM
 */
@Slf4j
@Component
// @ConditionalOnMissingBean 会识别到 参数中的类 的继承树，当容器中存在 该类 或 该类的子类 的类型时，@ConditionalOnMissingBean 返回false ，使得 @ConditionalOnMissingBean 标注的类 不执行。
// @ConditionalOnMissingBean(SimpleMQProducerService.class) // 但这玩意不能标注自身，似乎默认规则是先把Component标注的Bean放入一个Cache中，然后matchCondition(根据Cache中的内容)，不满足再从Cache中移除
@ConditionalOnMissingBean(ExtensionProducerService.class)
// One way to salve this problem is to define a subClass for extension
public sealed class SimpleMQProducerService extends XRabbitAbstractProducer permits ExtensionProducerService {

    @Autowired
    public void setAmqpTemplate(AmqpTemplate amqpTemplate) {
        if (amqpTemplate instanceof RabbitTemplate rabbitTemplate) initRabbitTemplate(rabbitTemplate);
        super.amqpTemplate = amqpTemplate;
    }

    /**
     * 定制RabbitTemplate
     * 1、服务收到消息就会回调
     * 1、spring.rabbitmq.publisher-confirm-type: correlated
     * 2、设置确认回调
     * 2、消息正确抵达队列就会进行回调
     * 1、spring.rabbitmq.publisher-returns: true
     * spring.rabbitmq.template.mandatory: true
     * 2、设置确认回调ReturnCallback
     * <p>
     * 3、消费端确认(保证每个消息都被正确消费，此时才可以broker删除这个消息)
     */
    public void initRabbitTemplate(RabbitTemplate rabbitTemplate) {

        /*
         * 1、只要消息抵达Broker就ack=true
         * correlationData：当前消息的唯一关联数据(这个是消息的唯一id)
         * ack：消息是否成功收到
         * cause：失败的原因
         */
        //设置确认回调
        rabbitTemplate.setConfirmCallback((correlationData, ack, cause) -> log.debug("confirm...correlationData[{}]==>ack:[{}]==>cause:[{}]", correlationData, ack, cause));


        /*
         * 只要消息没有投递给指定的队列，就触发这个失败回调
         * message：投递失败的消息详细信息
         * replyCode：回复的状态码
         * replyText：回复的文本内容
         * exchange：当时这个消息发给哪个交换机
         * routingKey：当时这个消息用哪个路邮键
         */
        rabbitTemplate.setReturnsCallback(returnedMessage -> log.warn("Fail Message[{}]==>replyCode[{}]==>replyText[{}]==>exchange[{}]==>routingKey[{}]",
                returnedMessage.getMessage(), returnedMessage.getReplyCode(), returnedMessage.getReplyText(), returnedMessage.getExchange(), returnedMessage.getRoutingKey()));
    }
}
