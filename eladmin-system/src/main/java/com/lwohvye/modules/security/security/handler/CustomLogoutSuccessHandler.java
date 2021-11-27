package com.lwohvye.modules.security.security.handler;

import com.lwohvye.utils.ResultUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Objects;

/**
 * @description 退出成功，后续处理逻辑
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
        ResultUtil.resultJson(response, HttpServletResponse.SC_OK, "退出成功");
    }

}
