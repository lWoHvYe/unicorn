/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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
