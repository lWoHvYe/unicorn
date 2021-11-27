package com.lwohvye.modules.security.security.handler;

import com.lwohvye.modules.security.service.dto.JwtUserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @description 处理退出逻辑
 * @date 2021/11/27 9:42 上午
 */
@Slf4j
public class CustomLogoutHandler implements LogoutHandler {
    @Override
    public void logout(HttpServletRequest request, HttpServletResponse response, Authentication authentication) {
        final JwtUserDto jwtUserDto = (JwtUserDto) authentication.getPrincipal();
        String username = jwtUserDto.getUsername();
        log.info("username: {}  is offline now", username);
    }
}
