package sample.interceptor;

import com.lwohvye.core.utils.StringUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

@Slf4j
@Component
public class AuthTokenInterceptor implements HandlerInterceptor {

    static final String AUTH_HEADER_NAME = "Authorization";

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) {
        MDC.remove(AUTH_HEADER_NAME);
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        var token = request.getHeader(AUTH_HEADER_NAME);
        if (StringUtils.isNotBlank(token)) {
            MDC.put(AUTH_HEADER_NAME, token);
        }
        return true;
    }
}
