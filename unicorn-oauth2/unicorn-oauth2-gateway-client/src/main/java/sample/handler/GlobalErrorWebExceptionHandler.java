/*
 *    Copyright (c) 2025-2026.  lWoHvYe(Hongyan Wang)
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

package sample.handler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.webflux.error.ErrorWebExceptionHandler;
import org.springframework.core.annotation.Order;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import sample.result.ResultInfo;
import tools.jackson.databind.ObjectMapper;

/**
 * 全局异常处理，这是另一种方式，常用的是另一种即extends AbstractErrorWebExceptionHandler
 */
@Slf4j
@Order(-2) // Ensure this has higher precedence than default handlers
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class GlobalErrorWebExceptionHandler implements ErrorWebExceptionHandler {

    private final ObjectMapper objectMapper;

    @Override
    public Mono<Void> handle(ServerWebExchange exchange, Throwable ex) {
        var response = exchange.getResponse();
        if (response.isCommitted()) {
            return Mono.error(ex);
        }
        response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        if (ex instanceof ResponseStatusException responseStatusException) {
            response.setStatusCode(responseStatusException.getStatusCode());
        }

        return response.writeWith(Mono.fromSupplier(() -> {
            var bufferFactory = response.bufferFactory();
            return bufferFactory.wrap(objectMapper.writeValueAsBytes(ResultInfo.failed(ex.getMessage())));
        }));
    }
}
