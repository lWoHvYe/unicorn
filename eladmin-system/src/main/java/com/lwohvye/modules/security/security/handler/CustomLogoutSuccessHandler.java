package com.lwohvye.modules.security.security.handler;

import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.utils.ResultUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @description 退出成功，后续处理逻辑
 * @date 2021/11/27 9:43 上午
 */
@Slf4j
public class CustomLogoutSuccessHandler implements LogoutSuccessHandler {
    @Override
    public void onLogoutSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
        final JwtUserDto jwtUserDto = (JwtUserDto) authentication.getPrincipal();
        String username = jwtUserDto.getUsername();
        log.info("username: {} logout success ", username);

        ResultUtil.resultJson(response, HttpServletResponse.SC_OK, "退出成功");
    }

}
