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
package com.lwohvye.modules.security.rest;

import cn.hutool.core.util.IdUtil;
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.annotation.rest.AnonymousDeleteMapping;
import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.annotation.rest.AnonymousPostMapping;
import com.lwohvye.config.RsaProperties;
import com.lwohvye.config.redis.AuthRedisUtils;
import com.lwohvye.config.redis.AuthSlaveRedisUtils;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.modules.kafka.service.KafkaProducerService;
import com.lwohvye.modules.security.config.bean.LoginCodeEnum;
import com.lwohvye.modules.security.config.bean.LoginProperties;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import com.lwohvye.modules.security.security.TokenProvider;
import com.lwohvye.modules.security.service.OnlineUserService;
import com.lwohvye.modules.security.service.dto.AuthUserDto;
import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.utils.redis.RedisUtils;
import com.lwohvye.utils.RsaUtils;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.StringUtils;
import com.lwohvye.utils.result.ResultInfo;
import com.wf.captcha.base.Captcha;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 * 授权、根据token获取用户详细信息
 */
@Slf4j
@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
@Api(tags = "系统：系统授权接口")
public class AuthorizationController {
    private final SecurityProperties properties;
    //    缓存
    private final RedisUtils redisUtils;
    //    鉴权用缓存
    private final AuthRedisUtils authRedisUtils;
    private final AuthSlaveRedisUtils authSlaveRedisUtils;
    private final OnlineUserService onlineUserService;
    private final TokenProvider tokenProvider;
    private final AuthenticationManagerBuilder authenticationManagerBuilder;
    //    队列生产者
    private final KafkaProducerService kafkaProducerService;

    @Resource
    private LoginProperties loginProperties;


    @ApiOperation("登录授权")
    @AnonymousPostMapping(value = "/login")
    // TODO: 2021/4/21 登陆失败限制、ip限制、手机验证码
    public ResponseEntity<Object> login(@Validated @RequestBody AuthUserDto authUser, HttpServletRequest request) throws Exception {

        var username = authUser.getUsername();
        String lockUserKey = username + "||authLocked||";
//        var lockUser = authRedisUtils.get(lockUserKey);
//        if (ObjectUtil.isNotNull(lockUser) && lockUser instanceof Collection col ? CollUtil.isNotEmpty(col) : ObjectUtil.isNotEmpty(lockUser)) {
//        if (ObjectUtil.isNotEmpty(lockUser)) {
        // TODO: 2021/4/22 改用延时消息队列来做。错误一定次数后，修改用户状态为锁定，然后延时消息。一小时后解除
//        if (authSlaveRedisUtils.hasKey(lockUserKey))
//            throw new BadRequestException("用户已被锁定，请稍后再试");

        // 密码解密
        String password = RsaUtils.decryptByPrivateKey(RsaProperties.privateKey, authUser.getPassword());
        // 查询验证码
        String code = (String) authSlaveRedisUtils.get(authUser.getUuid());
        // 清除验证码
        authRedisUtils.delete(authUser.getUuid());
        if (StringUtils.isBlank(code)) {
            throw new BadRequestException("验证码不存在或已过期");
        }
        if (StringUtils.isBlank(authUser.getCode()) || !authUser.getCode().equalsIgnoreCase(code)) {
            throw new BadRequestException("验证码错误");
        }
        UsernamePasswordAuthenticationToken authenticationToken =
                new UsernamePasswordAuthenticationToken(username, password);

        Authentication authentication;
        try {
            authentication = authenticationManagerBuilder.getObject().authenticate(authenticationToken);
        } catch (BadCredentialsException e) {
            String ip = StringUtils.getIp(request);
            var infoMap = new JSONObject();
            infoMap.put("ip", ip);
            infoMap.put("lockUserKey", lockUserKey);
            infoMap.put("username", username);
//            发送消息
            kafkaProducerService.sendCallbackMessage("auth-failed", infoMap.toJSONString());
            throw e;
        }

        SecurityContextHolder.getContext().setAuthentication(authentication);
        // 生成令牌与第三方系统获取令牌方式
        // UserDetails userDetails = userDetailsService.loadUserByUsername(userInfo.getUsername());
        // Authentication authentication = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
        // SecurityContextHolder.getContext().setAuthentication(authentication);
        String token = tokenProvider.createToken(authentication);
        final JwtUserDto jwtUserDto = (JwtUserDto) authentication.getPrincipal();
        // 保存在线信息
        onlineUserService.save(jwtUserDto, token, request);
        // 返回 token 与 用户信息
        Map<String, Object> authInfo = new HashMap<>(2) {
            {
                put("token", properties.getTokenStartWith() + token);
                put("user", jwtUserDto);
            }
        };
        if (loginProperties.isSingleLogin()) {
            //踢掉之前已经登录的token
            onlineUserService.checkLoginOnUser(username, token);
        }
//        用户登录成功后，写一条消息
        kafkaProducerService.sendCallbackMessage("auth-log", jwtUserDto.getUser().toString());
        return ResponseEntity.ok(authInfo);
    }

    @ApiOperation("获取用户信息")
    @GetMapping(value = "/info")
    public ResponseEntity<Object> getUserInfo() {
        return ResponseEntity.ok(SecurityUtils.getCurrentUser());
    }

    @ApiOperation("获取验证码")
    @AnonymousGetMapping(value = "/code")
    public ResponseEntity<Object> getCode() {
        // 获取运算的结果
        Captcha captcha = loginProperties.getCaptcha();
        String uuid = properties.getCodeKey() + IdUtil.simpleUUID();
        //当验证码类型为 arithmetic时且长度 >= 2 时，captcha.text()的结果有几率为浮点型
        String captchaValue = captcha.text();
        if (captcha.getCharType() - 1 == LoginCodeEnum.arithmetic.ordinal() && captchaValue.contains(".")) {
            captchaValue = captchaValue.split("\\.")[0];
        }
        // 保存
        authRedisUtils.set(uuid, captchaValue, loginProperties.getLoginCode().getExpiration(), TimeUnit.MINUTES);
        // 验证码信息
        Map<String, Object> imgResult = new HashMap<>(2) {
            {
                put("img", captcha.toBase64());
                put("uuid", uuid);
            }
        };
        return ResponseEntity.ok(imgResult);
    }

    @ApiOperation("退出登录")
    @AnonymousDeleteMapping(value = "/logout")
    public ResponseEntity<Object> logout(HttpServletRequest request) {
        onlineUserService.logout(tokenProvider.getToken(request));
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
