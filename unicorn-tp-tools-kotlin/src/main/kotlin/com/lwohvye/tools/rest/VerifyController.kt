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

import com.lwohvye.core.enums.CodeBiEnum
import com.lwohvye.core.enums.CodeEnum
import com.lwohvye.core.utils.result.ResultInfo
import com.lwohvye.tools.service.IEmailService
import com.lwohvye.tools.service.IVerifyService
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.tags.Tag
import org.springframework.web.bind.annotation.*
import java.util.*

/**
 * @author Zheng Jie
 * @date 2018-12-26
 */
@RestController
@RequestMapping("/api/code")
@Tag(name = "VerifyController", description = "工具：验证码管理")
class VerifyController(val verificationCodeService: IVerifyService, val emailService: IEmailService) {
    @PostMapping(value = ["/resetEmail"])
    @Operation(summary = "重置邮箱，发送验证码")
    fun resetEmail(@RequestParam email: String): ResultInfo<String> {
        val emailVo = verificationCodeService.sendEmail(email, CodeEnum.EMAIL_RESET_EMAIL_CODE.key)
        emailService.send(emailVo, emailService.find())
        return ResultInfo.success()
    }

    @PostMapping(value = ["/email/resetPass"])
    @Operation(summary = "重置密码，发送验证码")
    fun resetPass(@RequestParam email: String): ResultInfo<String> {
        val emailVo = verificationCodeService.sendEmail(email, CodeEnum.EMAIL_RESET_PWD_CODE.key)
        emailService.send(emailVo, emailService.find())
        return ResultInfo.success()
    }

    @GetMapping(value = ["/validated"])
    @Operation(summary = "验证码验证")
    fun validated(
        @RequestParam email: String,
        @RequestParam code: String,
        @RequestParam codeBi: Int?
    ): ResultInfo<String> {
        val biEnum = CodeBiEnum.find(codeBi)
        when (Objects.requireNonNull(biEnum)) {
            CodeBiEnum.ONE -> verificationCodeService.validated(
                CodeEnum.EMAIL_RESET_EMAIL_CODE.key + email, code)
            CodeBiEnum.TWO -> verificationCodeService.validated(
                CodeEnum.EMAIL_RESET_PWD_CODE.key + email, code)
            else -> {}
        }
        return ResultInfo.success()
    }
}
