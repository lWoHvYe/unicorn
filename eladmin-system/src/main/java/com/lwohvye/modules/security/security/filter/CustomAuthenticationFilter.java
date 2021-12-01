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
package com.lwohvye.modules.security.security.filter;

import com.lwohvye.config.RsaProperties;
import com.lwohvye.modules.security.service.dto.AuthUserDto;
import com.lwohvye.utils.*;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.SneakyThrows;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.InputStream;

/**
 * AuthenticationFilter that supports rest login(json login) and form login.
 *
 * @author chenhuanming
 */
public class CustomAuthenticationFilter extends UsernamePasswordAuthenticationFilter {

    @Override
    @SneakyThrows
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response) throws AuthenticationException {

        //attempt Authentication when Content-Type is json
        if (request.getContentType().equals(MediaType.APPLICATION_JSON_VALUE)) {
            UsernamePasswordAuthenticationToken authRequest;
            var redisUtils = SpringContextHolder.getBean(RedisUtils.class);

            try (InputStream is = request.getInputStream()) {
                var authUser = JsonUtils.toJavaObject(is, AuthUserDto.class);

                assert authUser != null;
                var username = authUser.getUsername();
                username = StringUtils.isNotBlank(username) ? username : "";

                var ip = StringUtils.getIp(request);
                var lockedIp = ip + "||authLocked||";
                // 当某ip多次登录失败导致用户锁定时，会同时锁定ip 15分钟
                if (redisUtils.hasKey(lockedIp)) {
                    ResultUtil.resultJson(response, HttpServletResponse.SC_BAD_REQUEST, "频繁访问，请稍后再试");
                    // return null即可返回，AbstractAuthenticationProcessingFilter将不再执行其他逻辑。后续走返回流程
                    return null;
                }
                // 密码解密
                var password = authUser.getPassword();
                password = StringUtils.isNotBlank(password) ? RsaUtils.decryptByPrivateKey(RsaProperties.privateKey, password) : "";

                // 查询验证码
                String code = (String) redisUtils.get(authUser.getUuid());
                // 清除验证码
                redisUtils.delete(authUser.getUuid());

                if (StringUtils.isBlank(code)) {
                    ResultUtil.resultJson(response, HttpServletResponse.SC_BAD_REQUEST, "验证码不存在或已过期");
                    return null;
                }

                if (StringUtils.isBlank(authUser.getCode()) || !authUser.getCode().equalsIgnoreCase(code)) {
                    ResultUtil.resultJson(response, HttpServletResponse.SC_BAD_REQUEST, "验证码错误");
                    return null;
                }

                authRequest = new UsernamePasswordAuthenticationToken(username, password);

                // 将用户名进去，若认证失败。另一侧从中取
                request.setAttribute("username", username);
                setDetails(request, authRequest);
            }
            // 在Spring Security中对用户进行认证的是AuthenticationManager，其只有一个方法，尝试对封装了认证信息的Authentication进行身份验证，如果成功，则返回完全填充的Authentication（包括授予的权限）
            // AuthenticationManager 只关注认证成功与否而并不关心具体的认证方式。对于这些具体认证方式是交给了AuthenticationProvider来负责。Manager将请求转发给具体的Provider实现类来做
            // UserDetailService, 用户认证通过Provider来做，所以Provider需要拿到系统已经保存的认证信息，获取用户信息的接口spring-security抽象成UserDetailService。
            // AuthenticationToken, 所有提交给AuthenticationManager的认证请求都会被封装成一个Token的实现
            // SecurityContext，当用户通过认证之后，就会为这个用户生成一个唯一的SecurityContext，里面包含用户的认证信息Authentication。
            // 通过SecurityContext我们可以获取到用户的标识Principle和授权信息GrantedAuthrity。在系统的任何地方只要通过SecurityHolder.getSecruityContext()就可以获取到SecurityContext。
            /*
             * 尝试对通过Authentication实例对象封装的身份信息进行验证。
             * 如果验证成功，则返回完全填充的Authentication对象（包括授予的权限）。
             *
             * AuthenticationManager 建议遵循以下的约定
             * 1，如果帐户被禁用并且AuthenticationManager可以测试此状态，则必须引发 DisabledException
             * 2，如果帐户被锁定并且并且AuthenticationManager可以测试帐户锁定，则必须抛出LockedException
             * 3，如果凭据不正确，则必须抛出BadCredentialsException
             * 虽然上述选项是可选的，但是 AuthenticationManager 必须始终测试凭据。
             * 我们应该上述顺序捕获这些异常，同时实现者也应按上述顺序抛出异常（即，如果帐户被禁用或锁定，
             * 则立即拒绝身份验证请求，并且不执行凭据测试过程），这可以防止根据禁用或锁定的帐户测试凭据。
             */
            return this.getAuthenticationManager().authenticate(authRequest);
        }
        //transmit it to UsernamePasswordAuthenticationFilter
        else {
            return super.attemptAuthentication(request, response);
        }
    }
}
