package com.lwohvye.modules.rabbitmq.domain;

import com.lwohvye.config.rabbitmq.DataOperatorEnum;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.concurrent.TimeUnit;

@Getter
@Setter
@ToString
@Accessors(chain = true)
public class AmqpMsgEntity {

    //    延时时间
    private Long expire;
    //    时间单位
    private TimeUnit timeUnit;

    //    消息类型
    private String msgType;

    //    操作类型
    private DataOperatorEnum operator;

    //    消息体
    private String msgData;

    //    其他额外属性
    private String extraData;

}
