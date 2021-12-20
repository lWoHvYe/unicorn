/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.tools.utils;

import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.tools.domain.vo.MailVo;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.utils.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.util.Date;
import java.util.Objects;

@Slf4j
@Component
public class MailUtils {

    @Autowired
    private JavaMailSenderImpl mailSender;

    public MailVo sendMail(MailVo mailVo) {
        try {
            checkMail(mailVo);
            sendMimeMail(mailVo);
            return mailVo;
        } catch (Exception e) {
            log.error("发送邮件失败:", e);
            mailVo.setStatus("fail");
            mailVo.setError(e.getMessage());
            return mailVo;
        }

    }

    public void sendMail(String to, String subject, String text) {
        var mailVo = new MailVo().setTo(StringUtils.isBlank(to) ? getMailDefaultTo() : to)
                .setSubject(subject).setText(text);
        sendMail(mailVo);
    }

    private void checkMail(MailVo mailVo) {
        if (StringUtils.isEmpty(mailVo.getTo())) {
            throw new BadRequestException("邮件收信人不能为空");
        }
        if (StringUtils.isEmpty(mailVo.getSubject())) {
            throw new BadRequestException("邮件主题不能为空");
        }
        if (StringUtils.isEmpty(mailVo.getText())) {
            throw new BadRequestException("邮件内容不能为空");
        }
    }

    private void sendMimeMail(MailVo mailVo) {
        try {
            var messageHelper = new MimeMessageHelper(mailSender.createMimeMessage(), true);
            mailVo.setFrom(getMailSendFrom());
            messageHelper.setFrom(mailVo.getFrom());
            messageHelper.setTo(mailVo.getTo().split(","));
            messageHelper.setSubject(mailVo.getSubject());
            messageHelper.setText(mailVo.getText());
            if (StringUtils.isNotBlank(mailVo.getCc()))
                messageHelper.setCc(mailVo.getCc().split(","));

            if (StringUtils.isNotBlank(mailVo.getBcc()))
                messageHelper.setCc(mailVo.getBcc().split(","));

            if (mailVo.getMultipartFiles() != null)
                for (MultipartFile multipartFile : mailVo.getMultipartFiles())
                    messageHelper.addAttachment(Objects.requireNonNull(multipartFile.getOriginalFilename()), multipartFile);


            if (ObjectUtil.isEmpty(mailVo.getSentDate())) {
                mailVo.setSentDate(new Date());
                messageHelper.setSentDate(mailVo.getSentDate());
            }
            mailSender.send(messageHelper.getMimeMessage());
            mailVo.setStatus("ok");
            log.info("发送邮件成功：{}->{}", mailVo.getFrom(), mailVo.getTo());
        } catch (Exception e) {
            throw new BadRequestException(e.getMessage());
        }
    }

    public String getMailSendFrom() {
        return mailSender.getJavaMailProperties().getProperty("from");
    }

    public String getMailDefaultTo() {
        return mailSender.getJavaMailProperties().getProperty("to-dev");
    }

}
