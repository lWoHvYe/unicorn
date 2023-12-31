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

package com.lwohvye.core.utils;

import cn.hutool.core.util.ReflectUtil;
import cn.hutool.extra.template.TemplateConfig;
import cn.hutool.extra.template.TemplateUtil;

import java.util.Map;

/**
 * 邮件通知统一抽象，可接入不同的服务
 *
 * @date 2022/3/12 11:34 AM
 */
public class MailAdapter {

    public static final String BEAN_NAME = "mailUtils"; // 提供服务的bean名称。后续可考虑支持配置

    /**
     * 邮件通知，需提供mailUtils，并有 sendMail(String to, String subject, String text) {} 方法
     *
     * @param to           /
     * @param subject      /
     * @param templateName /
     * @param paramsMap    /
     * @return java.lang.String
     * @date 2022/3/12 11:31 AM
     */
    public static String sendTemplatedMail(String to, String subject, String templateName, Map<String, Object> paramsMap) {

        try {
            // 基于模版生成正文
            var engine = TemplateUtil.createEngine(new TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH));
            var template = engine.getTemplate(templateName);
            var text = template.render(paramsMap);
            var mailUtils = SpringContextHolder.getBean(BEAN_NAME);
            ReflectUtil.invoke(mailUtils, "sendMail", to, subject, text);
        } catch (Exception ex) {
            return String.format("获取 %s 异常，原因 %s ，请确认是否引入相关模块", BEAN_NAME, ex.getMessage());
        }
        return "若未收到邮件，请检查邮箱配置";
    }
}
