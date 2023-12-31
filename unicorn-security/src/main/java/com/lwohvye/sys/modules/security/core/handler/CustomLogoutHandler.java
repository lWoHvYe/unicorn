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
package com.lwohvye.sys.modules.security.core.handler;

import com.lwohvye.sys.modules.security.core.TokenProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.logout.LogoutHandler;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.Objects;

/**
 * 处理退出逻辑
 *
 * @date 2021/11/27 9:42 上午
 */
@Slf4j
public record CustomLogoutHandler(TokenProvider tokenProvider) implements LogoutHandler {

    @Override
    public void logout(HttpServletRequest request, HttpServletResponse response, Authentication authentication) {
        // 根据过滤器的执行顺序。LogoutFilter在UsernamePasswordAuthenticationFilter之前执行。所以在这里时，是还没包办好的，要自己处理
        if (Objects.isNull(authentication)) {
            try {
                String token = tokenProvider.getToken(request);
                authentication = tokenProvider.getAuthentication(token);
            } catch (Exception ignored) {
                // 这里出异常后，不要再抛了，因为会被异常处理器处理，可能被重定向到logout，这就构成♻️了
            }
            // 如果还拿不到，就返回了。无情
            if (Objects.isNull(authentication))
                return;
            // 先放进去，因为后面还有个处理登出成功的要用
            // SecurityContextLogoutHandler在这之后执行，会清除信息
            // SecurityContextHolder.getContext().setAuthentication(authentication);
        }

        if (authentication.getPrincipal() instanceof UserDetails userDetails) {
            String username = userDetails.getUsername();
            log.info("username: {}  is offline now", username);
        }
    }
}
