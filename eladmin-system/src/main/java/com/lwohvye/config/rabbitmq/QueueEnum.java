package com.lwohvye.config.rabbitmq;


import lombok.Getter;

@Getter
public enum QueueEnum {
    /**
     * 消息通知队列
     */
    QUEUE_DATA_SYNC("data.sync.direct", "data.sync", "data.sync"),

    /**
     * 消息通知ttl队列
     */
    QUEUE_DATA_SYNC_TTL("data.sync.direct.ttl", "data.sync.ttl", "data.sync.ttl");

    /**
     * 交换名称
     */
    private String exchange;
    /**
     * 队列名称
     */
    private String name;
    /**
     * 路由键
     */
    private String routeKey;

    QueueEnum(String exchange, String name, String routeKey) {
        this.exchange = exchange;
        this.name = name;
        this.routeKey = routeKey;
    }
}
