package sample.interceptor;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Objects;

// propagate value from up-stream to down-stream for FeignClint call
@Slf4j
@Component
public class FeignClientHeaderModifierInterceptor implements RequestInterceptor {
    static final String AUTH_HEADER_NAME = "Authorization";

    @Override
    public void apply(RequestTemplate requestTemplate) {
        var requestAttributes = RequestContextHolder.getRequestAttributes();
        if (requestAttributes instanceof ServletRequestAttributes servletRequestAttributes) {
            var webRequest = servletRequestAttributes.getRequest();
            var userToken = webRequest.getHeader(AUTH_HEADER_NAME);
            if (Objects.nonNull(userToken) && !requestTemplate.headers().containsKey(AUTH_HEADER_NAME)) {
                requestTemplate.header(AUTH_HEADER_NAME, userToken);
            }
        }
    }
}
