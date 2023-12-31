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
import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.stereotype.Component;

/**
 * 在这里做通一的定义，子类通过继承拓展功能。这里就不要只用Component声明为bean了，不然当通过类型获取SimpleMQProducerService时，就会获取到多个
 *
 * @date 2022/3/19 2:37 PM
 */
@Component
// @ConditionalOnMissingBean 会识别到 参数中的类 的继承树，当容器中存在 该类 或 该类的子类 的类型时，@ConditionalOnMissingBean 返回false ，使得 @ConditionalOnMissingBean 标注的类 不执行。
// @ConditionalOnMissingBean(SimpleMQProducerService.class) // 但这玩意不能标注自身，似乎默认规则是先把Component标注的Bean放入一个Cache中，然后matchCondition(根据Cache中的内容)，不满足再从Cache中移除
@ConditionalOnMissingBean(ExtensionProducerService.class) // One way to salve this problem is to define a subClass for extension
public sealed class SimpleMQProducerService extends XRabbitAbstractProducer permits ExtensionProducerService {

    @Autowired
    public void setAmqpTemplate(AmqpTemplate amqpTemplate) {
        super.amqpTemplate = amqpTemplate;
    }

}
