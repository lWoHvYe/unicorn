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

import com.lwohvye.core.utils.result.ResultUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.Objects;

/**
 * 退出成功，后续处理逻辑
 *
 * @date 2021/11/27 9:43 上午
 */
@Slf4j
public class CustomLogoutSuccessHandler implements LogoutSuccessHandler {
    @Override
    public void onLogoutSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) {
        // 当前过滤器链的配置，这里是拿不到信息的
        if (Objects.isNull(authentication)) {
            // 也不要视图通过这个拿，前面有handler专门清理这个信息的
            // authentication = SecurityContextHolder.getContext().getAuthentication();
            log.warn(" van：oh,boy why you're runaway ");
        }
        ResultUtils.resultJson(response, HttpServletResponse.SC_OK, "退出成功");
    }

}
