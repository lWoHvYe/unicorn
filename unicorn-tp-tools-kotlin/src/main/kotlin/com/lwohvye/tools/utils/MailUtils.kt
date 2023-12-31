/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.tools.utils

import cn.hutool.core.util.ObjectUtil
import com.lwohvye.core.exception.BadRequestException
import com.lwohvye.core.utils.StringUtils
import com.lwohvye.tools.domain.vo.MailVo
import org.apache.logging.log4j.LogManager
import org.springframework.beans.factory.annotation.Value
import org.springframework.mail.javamail.JavaMailSender
import org.springframework.mail.javamail.MimeMessageHelper
import org.springframework.stereotype.Component
import java.util.*

@Component
class MailUtils(val mailSender: JavaMailSender) {
    companion object {
        private val log = LogManager.getLogger()
    }

    @Value("\${spring.mail.username}")
    private val mailFromUser: String? = null

    @Value("\${spring.mail.properties.to-dev}")
    private val mailToDev: String? = null
    fun sendMail(mailVo: MailVo): MailVo {
        return try {
            checkMail(mailVo)
            sendMimeMail(mailVo)
            mailVo
        } catch (e: Exception) {
            log.error("发送邮件失败:", e)
            mailVo.status = "fail"
            mailVo.error = e.message
            mailVo
        }
    }

    fun sendMail(to: String?, subject: String?, text: String?) {
        val mailVo = MailVo(null, null, to ?: mailToDev, subject, text)
        sendMail(mailVo)
    }

    private fun checkMail(mailVo: MailVo) {
        if (StringUtils.isEmpty(mailVo.to)) {
            throw BadRequestException("邮件收信人不能为空")
        }
        if (StringUtils.isEmpty(mailVo.subject)) {
            throw BadRequestException("邮件主题不能为空")
        }
        if (StringUtils.isEmpty(mailVo.text)) {
            throw BadRequestException("邮件内容不能为空")
        }
    }

    private fun sendMimeMail(mailVo: MailVo) {
        val messageHelper = MimeMessageHelper(mailSender.createMimeMessage(), true)
        if (StringUtils.isBlank(mailVo.from)) mailVo.from = mailFromUser
        mailVo.from?.let { messageHelper.setFrom(it) }
        mailVo.to?.split(",".toRegex())?.dropLastWhile { it.isEmpty() }?.toTypedArray()
            ?.let { messageHelper.setTo(it) }
        mailVo.subject?.let { messageHelper.setSubject(it) }
        mailVo.text?.let { messageHelper.setText(it) }
        if (StringUtils.isNotBlank(mailVo.cc)) mailVo.cc?.split(",".toRegex())?.dropLastWhile { it.isEmpty() }
            ?.toTypedArray()
            ?.let { messageHelper.setCc(it) }
        if (StringUtils.isNotBlank(mailVo.bcc)) mailVo.bcc?.split(",".toRegex())?.dropLastWhile { it.isEmpty() }
            ?.toTypedArray()?.let { messageHelper.setBcc(it) }
        if (mailVo.multipartFiles != null)
            for (multipartFile in mailVo.multipartFiles!!)
                messageHelper.addAttachment(Objects.requireNonNull(multipartFile.originalFilename), multipartFile)
        if (ObjectUtil.isEmpty(mailVo.sentDate)) {
            mailVo.sentDate = Date()
            mailVo.sentDate?.let { messageHelper.setSentDate(it) }
        }
        mailSender.send(messageHelper.mimeMessage)
        mailVo.status = "ok"
        log.info("发送邮件成功：{}->{}", mailVo.from, mailVo.to)
    }
}
