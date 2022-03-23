/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.modules.rabbitmq.config;

import com.lwohvye.config.LocalCoreConfig;
import org.springframework.amqp.core.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class RabbitMqConfig {

    // region 交换机
    // direct交换机
    public static final String DIRECT_SYNC_EXCHANGE = "sync_direct_exchange";
    // topic交换机
    public static final String TOPIC_SYNC_EXCHANGE = "sync_topic_exchange";
    // fanout交换机
    public static final String FANOUT_SIMPLE_EXCHANGE = "simple_fanout_exchange";
    // 延迟队列交换机
    public static final String DIRECT_SYNC_TTL_EXCHANGE = "sync_direct_ttl_exchange";
    // 延迟队列交换机-插件版
    public static final String DIRECT_SYNC_DELAY_EXCHANGE = "sync_delay_direct_exchange";
    public static final String TOPIC_SYNC_DELAY_EXCHANGE = "sync_delay_topic_exchange";
    // endregion

    // region 路由键
    public static final String DATA_SYNC_ROUTE_KEY = "data.sync";

    public static final String DATA_SYNC_TTL_ROUTE_KEY = "data.sync.ttl";

    public static final String DATA_COMMON_DELAY_ROUTE_KEY = "data.common.delay";

    public static final String AUTH_LOCAL_ROUTE_KEY = "auth.local";

    public static final String SP_SYNC_ROUTE_KEY = "sp.sync.x0x"; // 对应topic   sp.sync.*
    // endregion

    // region 队列
    public static final String DATA_SYNC_QUEUE = "data.sync.queue";

    public static final String DATA_SYNC_TTL_QUEUE = "data.sync.ttl.queue";

    public static final String DATA_COMMON_DELAY_QUEUE = "data.common.delay.queue";


    // endregion

    /**
     * 消费队列所绑定的交换机。直接匹配
     */
    @Bean
    DirectExchange dataSyncDirect() {
        return ExchangeBuilder
                .directExchange(DIRECT_SYNC_EXCHANGE)
                .durable(true)
                .build();
    }


    /**
     * 延迟消费队列所绑定的交换机
     */
    @Bean
    DirectExchange dataSyncTtlDirect() {
        return ExchangeBuilder
                .directExchange(DIRECT_SYNC_TTL_EXCHANGE)
                .durable(true)
                .build();
    }

    /**
     * 广播订阅。虽不如Topic灵活，但比较简单，将消息投递到所有绑定的队列，忽略Routing Key
     *
     * @return org.springframework.amqp.core.FanoutExchange
     * @date 2022/3/21 3:47 PM
     */
    @Bean
    FanoutExchange simpleFanoutDirect() {
        return ExchangeBuilder
                .fanoutExchange(FANOUT_SIMPLE_EXCHANGE)
                .durable(true)
                .build();
    }

    /**
     * 实际消费队列
     */
    @Bean
    public Queue dataSyncQueue() {
        return new Queue(DATA_SYNC_QUEUE); // 这种方式。默认持久化
    }

    /**
     * 延迟消费队列（死信队列）
     */
    @Bean
    public Queue dataSyncTtlQueue() {
        return QueueBuilder
                .durable(DATA_SYNC_TTL_QUEUE) // 这个durable，就是创建个持久化的
                .withArgument("x-dead-letter-exchange", DIRECT_SYNC_EXCHANGE)//到期后转发的交换机
                .withArgument("x-dead-letter-routing-key", DATA_SYNC_ROUTE_KEY)//到期后转发的路由键
                .build();
    }

    /**
     * 将消费队列绑定到交换机
     */
    @Bean
    Binding dataSyncBinding(DirectExchange dataSyncDirect, Queue dataSyncQueue) {
        return BindingBuilder
                .bind(dataSyncQueue)
                .to(dataSyncDirect)
                .with(DATA_SYNC_ROUTE_KEY);
    }

    /**
     * 将延迟消费队列绑定到交换机
     */
    @Bean
    Binding dataSyncTtlBinding(DirectExchange dataSyncTtlDirect, Queue dataSyncTtlQueue) {
        return BindingBuilder
                .bind(dataSyncTtlQueue)
                .to(dataSyncTtlDirect)
                .with(DATA_SYNC_TTL_ROUTE_KEY);
    }

    /**
     * 延迟队列-插件
     *
     * @return q
     */
    @Bean
    public Queue dataDelayQueue() {
        return QueueBuilder.durable(DATA_COMMON_DELAY_QUEUE).build();
    }

    /**
     * 延迟队列交换机-插件
     * direct模式
     *
     * @return ex
     */
    @Bean
    public CustomExchange dataDelayExchange() {
        Map<String, Object> args = new HashMap<>();
        args.put("x-delayed-type", "direct");
        return new CustomExchange(DIRECT_SYNC_DELAY_EXCHANGE, "x-delayed-message", true, false, args);
    }

    /**
     * 给延时队列绑定交换机-插件
     *
     * @return binding
     */
    @Bean
    public Binding delayBinding(CustomExchange dataDelayExchange, Queue dataDelayQueue) {
        return BindingBuilder
                .bind(dataDelayQueue)
                .to(dataDelayExchange)
                .with(DATA_COMMON_DELAY_ROUTE_KEY)
                .noargs();
    }

    @Bean
    public Binding delayBinding2(DirectExchange dataSyncDirect, Queue dataDelayQueue) {
        return BindingBuilder
                .bind(dataDelayQueue)
                .to(dataSyncDirect)
                .with(AUTH_LOCAL_ROUTE_KEY);
    }

    /**
     * topic交换机。支持路由通配符 *代表一个单词 #代表零个或多个单词。主题匹配
     *
     * @return org.springframework.amqp.core.TopicExchange
     * @date 2021/9/30 10:25 上午
     */
    @Bean
    public TopicExchange topicYExchange() {
        return ExchangeBuilder
                .topicExchange(TOPIC_SYNC_EXCHANGE)
                .durable(true)
                .build();
    }

    /**
     * 延迟队列交换机-插件
     * topic模式
     * https://github.com/rabbitmq/rabbitmq-delayed-message-exchange
     *
     * @return ex
     */
    @Bean
    public CustomExchange topicDelayExchange() {
        Map<String, Object> args = new HashMap<>();
        args.put("x-delayed-type", "topic");
        return new CustomExchange(TOPIC_SYNC_DELAY_EXCHANGE, "x-delayed-message", true, false, args);
    }

    /**
     * 该队列，集群各实例配置不相同，实现同一事件被各实例都消费，比如更新本地缓存
     *
     * @return org.springframework.amqp.core.Queue
     * @date 2022/3/8 10:22 AM
     */
    @Bean
    public Queue spSyncQueue() {
        return QueueBuilder
                .durable(LocalCoreConfig.SP_SYNC_DELAY_QUEUE)
                // 满足要求后转发的死信交换机及路由键
                .withArgument("x-dead-letter-exchange", DEAD_INFO_EXCHANGE)
                .withArgument("x-dead-letter-routing-key", DEAD_ROUTE_KEY)
                .build();
    }

    @Bean
    public Binding spSyncBinding(CustomExchange topicDelayExchange, Queue spSyncQueue) {
        return BindingBuilder.bind(spSyncQueue).to(topicDelayExchange).with("sp.sync.*").noargs();
    }

    // region 消费失败后，重试一定次数，之后转发到死信队列中

    public static final String EXCHANGE_TOPICS_INFORM = "exchange_topics_inform";
    private static final String DEAD_INFO_EXCHANGE = "x-dead-letter-exchange";

    public static final String QUEUE_INFORM_EMAIL = "queue_inform_email";
    public static final String ROUTE_KEY_EMAIL = "inform.#.email.#";

    public static final String DEAD_INFO_QUEUE = "dead_info_queue";
    public static final String DEAD_ROUTE_KEY = "dead_info_dev";

    //声明交换机
    @Bean
    public TopicExchange exchangeTopicsInform() {
        return ExchangeBuilder.topicExchange(EXCHANGE_TOPICS_INFORM).durable(true).build();
    }

    //声明QUEUE_INFORM_EMAIL队列，配置死信队列需要的参数
    @Bean
    public Queue queueInformEmail() {
//        Map<String, Object> map = new HashMap<>();
        // key固定，value根据业务
//        map.put("x-dead-letter-exchange", DEAD_INFO_EXCHANGE);
//        map.put("x-dead-letter-routing-key", DEAD_ROUTE_KEY);
//        return new Queue(QUEUE_INFORM_EMAIL, true, false, false, map);
        return QueueBuilder
                .durable(QUEUE_INFORM_EMAIL)
//                .withArguments(map)
                // 可以配置个时间，到时间后自动转发到死信队列 ms
                .withArgument("x-message-ttl", 10000)
                // 满足要求后转发的死信交换机及路由键
                .withArgument("x-dead-letter-exchange", DEAD_INFO_EXCHANGE)
                .withArgument("x-dead-letter-routing-key", DEAD_ROUTE_KEY)
                .build();
    }

    //ROUTE_KEY_EMAIL队列绑定交换机，指定routingKey
    @Bean
    public Binding bindingQueueInformEmail(Exchange exchangeTopicsInform, Queue queueInformEmail) {
        return BindingBuilder.bind(queueInformEmail).to(exchangeTopicsInform).with(ROUTE_KEY_EMAIL).noargs();
    }


    //以下为死信队列
    // 交换机
    @Bean
    public Exchange deadInfoExchange() {
        return ExchangeBuilder.directExchange(DEAD_INFO_EXCHANGE).durable(true).build();
    }

    @Bean
    public Queue deadInfoQueue() {
        return QueueBuilder.durable(DEAD_INFO_QUEUE).build();
    }

    @Bean
    public Binding deadInfoQueueBind(Exchange deadInfoExchange, Queue deadInfoQueue) {
        return BindingBuilder.bind(deadInfoQueue).to(deadInfoExchange).with(DEAD_ROUTE_KEY).noargs();
    }

    // endregion
}
