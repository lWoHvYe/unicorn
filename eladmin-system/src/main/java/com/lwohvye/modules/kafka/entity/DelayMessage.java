package com.lwohvye.modules.kafka.entity;

import cn.hutool.core.util.IdUtil;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.concurrent.TimeUnit;

@Data
@Accessors(chain = true)
public class DelayMessage {
    /**
     * 事件唯一ID,用于去重检查
     */
    private String eventId = IdUtil.fastSimpleUUID();

    /**
     * 有效期
     */
    private Long time;

    /**
     * 单位
     */
    private TimeUnit unit;

    /**
     * 内容
     */
    private String context;

    /**
     * 真实Topic
     */
    private String actualTopic;
}
