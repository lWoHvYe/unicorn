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


import lombok.Getter;

@Getter
// TODO: 2021/9/12 交换机、路由键、队列 几者间的关系很复杂，这种一对一对一的定义方式，有待进行优化
public enum QueueEnum {
    /**
     * 消息通知队列
     */
    QUEUE_DATA_SYNC("data.sync.direct", "data.sync", "data.sync"),

    /**
     * 消息通知ttl队列
     */
    QUEUE_DATA_SYNC_TTL("data.sync.direct.ttl", "data.sync.ttl", "data.sync.ttl"),

    /**
     * 延迟插件队列
     */
    QUEUE_DATA_SYNC_DELAY("data.sync.delay", "data.common.delay", "data.common.delay");

    /**
     * 交换名称
     */
    private String exchange;
    /**
     * 队列名称
     */
    private String queueName;
    /**
     * 路由键
     */
    private String routeKey;

    QueueEnum(String exchange, String queueName, String routeKey) {
        this.exchange = exchange;
        this.queueName = queueName;
        this.routeKey = routeKey;
    }
}
