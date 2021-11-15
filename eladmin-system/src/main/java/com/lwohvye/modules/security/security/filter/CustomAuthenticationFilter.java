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
            return this.getAuthenticationManager().authenticate(authRequest);
        }
        //transmit it to UsernamePasswordAuthenticationFilter
        else {
            return super.attemptAuthentication(request, response);
        }
    }
}
