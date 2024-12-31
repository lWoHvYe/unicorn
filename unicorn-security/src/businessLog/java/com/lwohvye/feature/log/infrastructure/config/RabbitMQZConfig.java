/*
 *    Copyright (c) 2024-2025.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.feature.log.infrastructure.config;

import org.springframework.amqp.core.*;
import org.springframework.context.annotation.Bean;

public class RabbitMQZConfig {

    public static final String DIRECT_SYNC_EXCHANGE = "sync_direct_exchange";

    public static final String BUSINESS_LOG_ROUTE_KEY = "business.log";

    public static final String BUSINESS_LOG_QUEUE = "business.log.queue";

    // region 业务相关log

    @Bean
    public Queue businessLogQueue() {
        return QueueBuilder.durable(BUSINESS_LOG_QUEUE).build();
    }

    @Bean
    public Binding businessLogBinding(DirectExchange dataSyncDirect, Queue businessLogQueue) {
        return BindingBuilder
                .bind(businessLogQueue)
                .to(dataSyncDirect)
                .with(BUSINESS_LOG_ROUTE_KEY);
    }
    // endregion
}
