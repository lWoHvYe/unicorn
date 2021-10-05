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
package com.lwohvye.config.rabbitmq;

import org.springframework.amqp.core.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class RabbitMqConfig {

    public static final String TOPIC_SYNC_EXCHANGE = "sync_topic_exchange";

    public static final String TOPIC_SYNC_DELAY_EXCHANGE = "sync_delay_topic_exchange";

    /**
     * 消费队列所绑定的交换机
     */
    @Bean
    DirectExchange dataSyncDirect() {
        return ExchangeBuilder
                .directExchange(QueueEnum.QUEUE_DATA_SYNC.getExchange())
                .durable(true)
                .build();
    }


    /**
     * 延迟消费队列所绑定的交换机
     */
    @Bean
    DirectExchange dataSyncTtlDirect() {
        return ExchangeBuilder
                .directExchange(QueueEnum.QUEUE_DATA_SYNC_TTL.getExchange())
                .durable(true)
                .build();
    }


    /**
     * 实际消费队列
     */
    @Bean
    public Queue dataSyncQueue() {
        return new Queue(QueueEnum.QUEUE_DATA_SYNC.getQueueName());
    }

    /**
     * 延迟消费队列（死信队列）
     */
    @Bean
    public Queue dataSyncTtlQueue() {
        return QueueBuilder
                .durable(QueueEnum.QUEUE_DATA_SYNC_TTL.getQueueName())
                .withArgument("x-dead-letter-exchange", QueueEnum.QUEUE_DATA_SYNC.getExchange())//到期后转发的交换机
                .withArgument("x-dead-letter-routing-key", QueueEnum.QUEUE_DATA_SYNC.getRouteKey())//到期后转发的路由键
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
                .with(QueueEnum.QUEUE_DATA_SYNC.getRouteKey());
    }

    /**
     * 将延迟消费队列绑定到交换机
     */
    @Bean
    Binding dataSyncTtlBinding(DirectExchange dataSyncTtlDirect, Queue dataSyncTtlQueue) {
        return BindingBuilder
                .bind(dataSyncTtlQueue)
                .to(dataSyncTtlDirect)
                .with(QueueEnum.QUEUE_DATA_SYNC_TTL.getRouteKey());
    }

    /**
     * 延迟队列-插件
     *
     * @return q
     */
    @Bean
    public Queue dataDelayQueue() {
        return QueueBuilder.durable(QueueEnum.QUEUE_DATA_SYNC_DELAY.getQueueName()).build();
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
        return new CustomExchange(QueueEnum.QUEUE_DATA_SYNC_DELAY.getExchange(), "x-delayed-message", true, false, args);
    }

    /**
     * 给延时队列绑定交换机-插件
     *
     * @return binding
     */
    @Bean
    public Binding delayBinding(Queue dataDelayQueue, CustomExchange dataDelayExchange) {
        return BindingBuilder
                .bind(dataDelayQueue)
                .to(dataDelayExchange)
                .with(QueueEnum.QUEUE_DATA_SYNC_DELAY.getRouteKey())
                .noargs();
    }

    /**
     * @return org.springframework.amqp.core.TopicExchange
     * @description topic交换机。支持路由通配符 *代表一个单词 #代表零个或多个单词
     * @author Hongyan Wang
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

    // region 消费失败后，重试一定次数，之后转发到死信队列中

    public static final String EXCHANGE_TOPICS_INFORM = "exchange_topics_inform";
    private static final String DEAD_INFO_EXCHANGE = "x-dead-letter-exchange";

    public static final String QUEUE_INFORM_EMAIL = "queue_inform_email";
    public static final String ROUTE_KEY_EMAIL = "inform.#.email.#";

    public static final String DEAD_INFO_QUEUE = "dead_info_queue";
    public static final String DEAD_ROUTE_KEY = "dead_info_dev";

    //声明交换机
    @Bean
    public Exchange exchangeTopicsInform() {
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
    public Binding bindingQueueInformEmail(Queue queueInformEmail, Exchange exchangeTopicsInform) {
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
    public Binding deadInfoQueueBind(Queue deadInfoQueue, Exchange deadInfoExchange) {
        return BindingBuilder.bind(deadInfoQueue).to(deadInfoExchange).with(DEAD_ROUTE_KEY).noargs();
    }

    // endregion
}
