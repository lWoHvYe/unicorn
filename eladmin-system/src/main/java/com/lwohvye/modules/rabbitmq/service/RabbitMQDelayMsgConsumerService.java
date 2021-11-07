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
package com.lwohvye.modules.rabbitmq.service;

import cn.hutool.core.lang.Dict;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.extra.template.Template;
import cn.hutool.extra.template.TemplateConfig;
import cn.hutool.extra.template.TemplateEngine;
import cn.hutool.extra.template.TemplateUtil;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.config.rabbitmq.RabbitMqConfig;
import com.lwohvye.domain.vo.MailVo;
import com.lwohvye.modules.rabbitmq.domain.AmqpMsgEntity;
import com.lwohvye.modules.system.service.local.AuthMQService;
import com.lwohvye.utils.MailUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
// 监听延迟插件相关队列的消息
@RabbitListener(queues = RabbitMqConfig.DATA_COMMON_DELAY_QUEUE)
// 支持多种配置方式 property placeholders and SpEL  https://docs.spring.io/spring-amqp/docs/current/reference/html/#choose-container
//@RabbitListener(queues = "#{'${property.with.comma.delimited.queue.names}'.split(',')}" )
// 还可调用静态和非静态方法
// SpEL https://www.lwohvye.com/2021/06/11/spring-%e8%a1%a8%e8%be%be%e5%bc%8f%e8%af%ad%e8%a8%80-spel/
//@RabbitListener(queues = "#{T(全类名).静态方法名}")
//@RabbitListener(queues = "#{beanName.方法名}")

// 当将@RabbitListener注解放在类上时，一些情况下会报 org.springframework.amqp.AmqpException: No method found for class [B
// 相关原因参见：https://jira.spring.io/browse/AMQP-573
// 可使用将@RabbitListener放到方法上的方式。因为类型推断只适用于方法级别的@RabbitListener
// 使用@Payload注解可以获取消息中的body信息，使用@Headers注解可以获取消息中的headers信息，也可以使用@Header获取单个header属性

// @RabbitListener 可以标注在类上面，需配合 @RabbitHandler 注解一起使用
// @RabbitListener 标注在类上面表示当有收到消息的时候，就交给 @RabbitHandler 的方法处理，具体使用哪个方法处理，根据 MessageConverter 转换后的参数类型
public class RabbitMQDelayMsgConsumerService {

    @Autowired
    private AuthMQService authMQService;

    @Autowired
    private MailUtils mailUtils;

    @RabbitHandler
    public void handle(String amqpMsgEntityStr) {
        var amqpMsgEntity = JSONObject.parseObject(amqpMsgEntityStr, AmqpMsgEntity.class);
        var msgType = amqpMsgEntity.getMsgType();
        var msgData = amqpMsgEntity.getMsgData();
        try {
            if (StrUtil.isBlank(msgData))
                return;
            // 鉴权类
            if (ObjectUtil.equals(msgType, "auth")) {
                var extraData = amqpMsgEntity.getExtraData();
                if (StrUtil.isNotBlank(extraData))
                    ReflectUtil.invoke(authMQService, extraData, msgData);
            }
        } catch (Exception e) {
            log.error(" Consume Msg Error, Reason: {} || Msg detail: {} ", e.getMessage(), amqpMsgEntityStr);
            var mailVo = new MailVo().setTo(mailUtils.getMailDefaultTo()).setSubject("Consume Msg Error" + this.getClass().getSimpleName());
            // 基于模版生成正文
            TemplateEngine engine = TemplateUtil.createEngine(new TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH));
            Template template = engine.getTemplate("email/noticeEmail.ftl");
            var text = template.render(Dict.create().setIgnoreNull("errMsg", e.getMessage()));

            mailVo.setText(text);
            // 邮件通知
            mailUtils.sendMail(mailVo);
        } finally {
            log.info("Consume Msg,Msg type: {}, -+- ,Msg detail: {}", msgType, amqpMsgEntityStr);
            // 处理完成，根据结果记录相关表（看业务需求）。若处理报错，需邮件通知
        }
    }

}

