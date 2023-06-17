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
package com.lwohvye.tools.rest

import com.lwohvye.core.annotation.log.OprLog
import com.lwohvye.tools.domain.EmailConfig
import com.lwohvye.tools.domain.vo.EmailVo
import com.lwohvye.tools.service.IEmailService
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.tags.Tag
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.validation.annotation.Validated
import org.springframework.web.bind.annotation.*

/**
 * 发送邮件
 *
 * @author 郑杰
 * @date 2018/09/28 6:55:53
 */
@RestController
@RequestMapping("api/email")
@Tag(name = "EmailController", description = "工具：邮件管理")
class EmailController(val emailService: IEmailService) {
    @GetMapping
    fun queryConfig(): EmailConfig? {
        return emailService.find()
    }

    @OprLog("配置邮件")
    @PutMapping
    @Operation(summary = "配置邮件")
    @Throws(Exception::class)
    fun updateConfig(@Validated @RequestBody emailConfig: EmailConfig): ResponseEntity<String> {
        emailService.config(emailConfig, emailService.find())
        return ResponseEntity(HttpStatus.OK)
    }

    @OprLog("发送邮件")
    @PostMapping
    @Operation(summary = "发送邮件")
    fun sendEmail(@Validated @RequestBody emailVo: EmailVo?): ResponseEntity<String> {
        emailService.send(emailVo, emailService.find())
        return ResponseEntity(HttpStatus.OK)
    }
}
