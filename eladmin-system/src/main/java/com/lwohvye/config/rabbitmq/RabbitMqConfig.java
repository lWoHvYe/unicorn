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

@Configuration
public class RabbitMqConfig {
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
        return new Queue(QueueEnum.QUEUE_DATA_SYNC.getName());
    }

    /**
     * 延迟消费队列（死信队列）
     */
    @Bean
    public Queue dataSyncTtlQueue() {
        return QueueBuilder
                .durable(QueueEnum.QUEUE_DATA_SYNC_TTL.getName())
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
}
