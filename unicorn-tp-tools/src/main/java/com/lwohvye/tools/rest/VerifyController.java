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
package com.lwohvye.tools.rest;

import com.lwohvye.tools.domain.vo.EmailVo;
import com.lwohvye.tools.service.IVerifyService;
import com.lwohvye.tools.service.IEmailService;
import com.lwohvye.core.utils.enums.CodeBiEnum;
import com.lwohvye.core.utils.enums.CodeEnum;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

/**
 * @author Zheng Jie
 * @date 2018-12-26
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/code")
@Tag(name = "VerifyController", description = "工具：验证码管理")
public class VerifyController {

    private final IVerifyService verificationCodeService;
    private final IEmailService emailService;

    @PostMapping(value = "/resetEmail")
    @Operation(summary = "重置邮箱，发送验证码")
    public ResponseEntity<ResultInfo<String>> resetEmail(@RequestParam String email) {
        EmailVo emailVo = verificationCodeService.sendEmail(email, CodeEnum.EMAIL_RESET_EMAIL_CODE.getKey());
        emailService.send(emailVo, emailService.find());
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @PostMapping(value = "/email/resetPass")
    @Operation(summary = "重置密码，发送验证码")
    public ResponseEntity<ResultInfo<String>> resetPass(@RequestParam String email) {
        EmailVo emailVo = verificationCodeService.sendEmail(email, CodeEnum.EMAIL_RESET_PWD_CODE.getKey());
        emailService.send(emailVo, emailService.find());
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @GetMapping(value = "/validated")
    @Operation(summary = "验证码验证")
    public ResponseEntity<ResultInfo<String>> validated(@RequestParam String email, @RequestParam String code, @RequestParam Integer codeBi) {
        CodeBiEnum biEnum = CodeBiEnum.find(codeBi);
        switch (Objects.requireNonNull(biEnum)) {
            case ONE:
                verificationCodeService.validated(CodeEnum.EMAIL_RESET_EMAIL_CODE.getKey() + email, code);
                break;
            case TWO:
                verificationCodeService.validated(CodeEnum.EMAIL_RESET_PWD_CODE.getKey() + email, code);
                break;
            default:
                break;
        }
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
