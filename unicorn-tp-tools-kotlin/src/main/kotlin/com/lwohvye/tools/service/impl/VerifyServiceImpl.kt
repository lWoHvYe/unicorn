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

import cn.hutool.core.lang.Dict
import cn.hutool.core.util.RandomUtil
import cn.hutool.extra.template.TemplateConfig
import cn.hutool.extra.template.TemplateUtil
import com.lwohvye.core.exception.BadRequestException
import com.lwohvye.core.utils.redis.RedisUtils
import com.lwohvye.tools.domain.vo.EmailVo
import com.lwohvye.tools.service.IVerifyService
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

/**
 * @author Zheng Jie
 * @date 2018-12-26
 */
@Service
class VerifyServiceImpl(val redisUtils: RedisUtils) : IVerifyService {
    @Value("\${code.expiration:250}")
    private val expiration: Long? = null

    @Transactional(rollbackFor = [Exception::class])
    override fun sendEmail(email: String, key: String): EmailVo {
        val emailVo: EmailVo
        val content: String
        val redisKey = key + email
        // 如果不存在有效的验证码，就创建一个新的
        val engine = TemplateUtil.createEngine(TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH))
        val template = engine.getTemplate("email/email.ftl")
        val oldCode = redisUtils.get(redisKey)
        if (oldCode == null) {
            val code = RandomUtil.randomNumbers(6)
            // 存入缓存
            if (!redisUtils.set(redisKey, code, expiration!!)) {
                throw BadRequestException("服务异常，请联系网站负责人")
            }
            content = template.render(Dict.create().set("code", code))
            emailVo = EmailVo(listOf(email), "EL-ADMIN后台管理系统", content)
            // 存在就再次发送原来的验证码
        } else {
            content = template.render(Dict.create().set("code", oldCode))
            emailVo = EmailVo(listOf(email), "EL-ADMIN后台管理系统", content)
        }
        return emailVo
    }

    override fun validated(key: String?, code: String) {
        val value = redisUtils[key]
        if (value == null || value.toString() != code) {
            throw BadRequestException("无效验证码")
        } else {
            redisUtils.delete(key)
        }
    }
}
