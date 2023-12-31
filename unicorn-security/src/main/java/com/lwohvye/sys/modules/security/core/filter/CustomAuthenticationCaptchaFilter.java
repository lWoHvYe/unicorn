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
package com.lwohvye.sys.modules.security.core.filter;

import com.anji.captcha.model.vo.CaptchaVO;
import com.anji.captcha.service.CaptchaService;
import com.lwohvye.core.utils.SpringContextHolder;
import com.lwohvye.core.utils.redis.RedisUtils;
import com.lwohvye.core.utils.result.ResultUtils;
import com.lwohvye.sys.modules.security.service.dto.AuthUserDto;
import jakarta.servlet.http.HttpServletResponse;

import java.util.Objects;

/**
 * AuthenticationFilter that supports rest login(json login) and form login.
 *
 * @author chenhuanming
 */
public class CustomAuthenticationCaptchaFilter extends CustomAuthenticationFilter {

    private CaptchaService captchaService;

    public void doRegister() {
        if (Objects.isNull(redisUtils)) redisUtils = SpringContextHolder.getBean(RedisUtils.class);
        if (Objects.isNull(captchaService)) captchaService = SpringContextHolder.getBean(CaptchaService.class);
    }

    @Override
    protected boolean extraVerifyFailed(HttpServletResponse response, AuthUserDto authUser) {
        // 前端回传二次验证参数
        var captchaVO = new CaptchaVO();
        captchaVO.setCaptchaVerification(authUser.getCaptchaVerification());
        // 对参数进行验证
        var verifyRes = captchaService.verification(captchaVO);
        if (!verifyRes.isSuccess()) {
            //验证码校验失败，返回信息告诉前端
            ResultUtils.resultJson(response, HttpServletResponse.SC_BAD_REQUEST, verifyRes.getRepMsg());
            return true;
        }
        return false;
    }
}
