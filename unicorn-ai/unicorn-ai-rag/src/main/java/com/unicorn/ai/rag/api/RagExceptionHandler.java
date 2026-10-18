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

package com.unicorn.ai.rag.api;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@Slf4j
@RestControllerAdvice
public class RagExceptionHandler {

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<RagError> badRequest(IllegalArgumentException exception) {
        return ResponseEntity.badRequest().body(new RagError("BAD_REQUEST", exception.getMessage()));
    }

    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<RagError> unavailable(IllegalStateException exception) {
        return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                .body(new RagError("FEATURE_UNAVAILABLE", exception.getMessage()));
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<RagError> failed(Exception exception) {
        log.error("RAG 调用失败", exception);
        return ResponseEntity.status(HttpStatus.BAD_GATEWAY)
                .body(new RagError("RAG_FAILED", rootMessage(exception)));
    }

    private String rootMessage(Throwable throwable) {
        var current = throwable;
        while (current.getCause() != null) {
            current = current.getCause();
        }
        return current.getMessage() == null ? current.getClass().getSimpleName() : current.getMessage();
    }

    public record RagError(String code, String message) {
    }
}
