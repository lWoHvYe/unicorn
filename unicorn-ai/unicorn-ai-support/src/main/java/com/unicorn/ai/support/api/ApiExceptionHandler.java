/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.unicorn.ai.support.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import com.unicorn.ai.support.ratelimit.RateLimitException;
import com.unicorn.ai.support.chat.ChatRequestConflictException;
import com.unicorn.ai.support.model.ModelCallException;
import com.unicorn.ai.tools.policy.ToolConflictException;

import java.util.NoSuchElementException;
import java.util.UUID;

@RestControllerAdvice
public class ApiExceptionHandler {

    private static final Logger log = LoggerFactory.getLogger(ApiExceptionHandler.class);

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ApiError> badRequest(IllegalArgumentException exception) {
        return error(HttpStatus.BAD_REQUEST, "BAD_REQUEST", exception.getMessage());
    }

    @ExceptionHandler(NoSuchElementException.class)
    public ResponseEntity<ApiError> notFound(NoSuchElementException exception) {
        return error(HttpStatus.NOT_FOUND, "NOT_FOUND", exception.getMessage());
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ApiError> forbidden(AccessDeniedException exception) {
        return error(HttpStatus.FORBIDDEN, "FORBIDDEN", "没有权限访问该资源");
    }

    @ExceptionHandler(ChatRequestConflictException.class)
    public ResponseEntity<ApiError> chatConflict(ChatRequestConflictException exception) {
        return error(HttpStatus.CONFLICT, "CHAT_REQUEST_CONFLICT", exception.getMessage());
    }

    @ExceptionHandler(ToolConflictException.class)
    public ResponseEntity<ApiError> conflict(ToolConflictException exception) {
        return error(HttpStatus.CONFLICT, "TOOL_CONFLICT", exception.getMessage());
    }

    @ExceptionHandler(RateLimitException.class)
    public ResponseEntity<ApiError> rateLimited(RateLimitException exception) {
        return error(HttpStatus.TOO_MANY_REQUESTS, "RATE_LIMITED", exception.getMessage());
    }

    @ExceptionHandler(ModelCallException.class)
    public ResponseEntity<ApiError> modelFailure(ModelCallException exception) {
        log.warn("Model call failed: {}", exception.code());
        var status = "MODEL_TIMEOUT".equals(exception.code())
                ? HttpStatus.GATEWAY_TIMEOUT : HttpStatus.SERVICE_UNAVAILABLE;
        return error(status, exception.code(), "模型服务暂时不可用，请稍后重试");
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ApiError> failed(Exception exception) {
        log.error("Unhandled AI support request failure", exception);
        return error(HttpStatus.BAD_GATEWAY, "AI_SUPPORT_FAILED", "服务暂时不可用，请稍后重试");
    }

    private ResponseEntity<ApiError> error(HttpStatus status, String code, String message) {
        return ResponseEntity.status(status).body(new ApiError(code,
                message == null || message.isBlank() ? status.getReasonPhrase() : message, traceId()));
    }

    private String traceId() {
        var trace = MDC.get("traceId");
        return trace == null || trace.isBlank() ? UUID.randomUUID().toString() : trace;
    }

    public record ApiError(String code, String message, String traceId) {
    }
}
