/*
 *  Copyright 2019-2020 Zheng Jie
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
package com.lwohvye.tools.service.impl

import cn.hutool.extra.mail.MailAccount
import cn.hutool.extra.mail.MailUtil
import com.lwohvye.core.exception.BadRequestException
import com.lwohvye.core.utils.EncryptUtils
import com.lwohvye.tools.domain.EmailConfig
import com.lwohvye.tools.domain.vo.EmailVo
import com.lwohvye.tools.repository.EmailRepository
import com.lwohvye.tools.service.IEmailService
import org.springframework.cache.annotation.CacheConfig
import org.springframework.cache.annotation.CachePut
import org.springframework.cache.annotation.Cacheable
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

/**
 * @author Zheng Jie
 * @date 2018-12-26
 */
@Service
@CacheConfig(cacheNames = ["email"])
class EmailServiceImpl(val emailRepository: EmailRepository) : IEmailService {

    @CachePut(key = "'config'")
    @Transactional(rollbackFor = [Exception::class])
    @Throws(Exception::class)
    override fun config(emailConfig: EmailConfig, old: EmailConfig?): EmailConfig {
        emailConfig.id = 1L
        if (emailConfig.pass != old?.pass) {
            // 对称加密
            emailConfig.pass = EncryptUtils.aesEncrypt(emailConfig.pass)
        }
        return emailRepository.save(emailConfig)
    }

    @Transactional(rollbackFor = [Exception::class], readOnly = true)
    @Cacheable(key = "'config'")
    override fun find(): EmailConfig? {
        val emailConfig = emailRepository.findById(1L)
        return emailConfig.orElseGet { EmailConfig() }
    }

    @Transactional(rollbackFor = [Exception::class])
    override fun send(emailVo: EmailVo?, emailConfig: EmailConfig?) {
        if (emailConfig?.id == null) {
            throw BadRequestException("请先配置，再操作")
        }
        // 封装
        val account = MailAccount()
        // 设置用户
        val user = emailConfig.fromUser?.split("@".toRegex())?.dropLastWhile { it.isEmpty() }?.toTypedArray()?.get(0)
        account.user = user
        account.host = emailConfig.host
        account.port = emailConfig.port?.toInt()
        account.isAuth = true
        // 对称解密
        account.pass = EncryptUtils.aesDecrypt(emailConfig.pass)
        //        部分邮箱 user和fromUser需一致
        account.from = " ${emailConfig.user} <${emailConfig.fromUser}>"
        // ssl方式发送-----不使用
//        account.setSslEnable(true);
        account.isSslEnable = false
        // 使用STARTTLS安全连接
        account.isStarttlsEnable = true
        val content = emailVo?.content
        // 发送
        MailUtil.send(account, emailVo?.tos, emailVo?.subject, content, true)
    }

    @Transactional(rollbackFor = [Exception::class], readOnly = true)
    override fun send(tos: List<String>?, subject: String?, content: String?) {
        val emailVo = EmailVo(tos, subject, content)
        send(emailVo, find())
    }
}
