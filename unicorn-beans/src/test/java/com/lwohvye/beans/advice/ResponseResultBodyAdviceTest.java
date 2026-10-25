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

package com.lwohvye.beans.advice;

import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.result.ResultInfo;
import org.junit.jupiter.api.Test;
import org.springframework.core.MethodParameter;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.http.server.ServletServerHttpResponse;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.context.request.WebRequest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class ResponseResultBodyAdviceTest {

    private final ResponseResultBodyAdvice advice = new ResponseResultBodyAdvice();

    @Test
    void stringBodyIsWrappedAsJson() throws NoSuchMethodException {
        var response = new ServletServerHttpResponse(new MockHttpServletResponse());
        var result = advice.beforeBodyWrite("hello",
                new MethodParameter(getClass().getDeclaredMethod("stringBody"), -1),
                MediaType.TEXT_PLAIN,
                StringHttpMessageConverter.class,
                new ServletServerHttpRequest(new MockHttpServletRequest()),
                response);

        assertInstanceOf(String.class, result);
        assertTrue(((String) result).contains("\"result\":\"hello\""));
        assertEquals(MediaType.APPLICATION_JSON, response.getHeaders().getContentType());
    }

    private String stringBody() {
        return "";
    }

    @Test
    void fallbackExceptionDoesNotLeakMessage() {
        ResponseEntity<ResultInfo<?>> response = advice.handleException(
                new RuntimeException("jdbc:mysql://internal-host/secret"), mock(WebRequest.class));

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        assertFalse(response.getBody().getDescription().contains("internal-host"));
        assertTrue(response.getBody().getDescription().contains("traceId"));
    }

    @Test
    void illegalArgumentDoesNotLeakMessage() {
        ResponseEntity<ResultInfo<?>> response = advice.handleServletException(
                new IllegalArgumentException("internal detail"), mock(WebRequest.class));

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertEquals("请求参数不合法", response.getBody().getDescription());
        assertEquals(400, response.getBody().getBusinessCode());
    }

    @Test
    void badRequestKeepsBusinessMessage() {
        ResponseEntity<ResultInfo<?>> response = advice.handleServletException(
                new BadRequestException("用户名已存在"), mock(WebRequest.class));

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertEquals("用户名已存在", response.getBody().getDescription());
        assertEquals(400, response.getBody().getBusinessCode());
    }

    @Test
    void dataIntegrityViolationDoesNotLeakSql() {
        ResponseEntity<ResultInfo<?>> response = advice.handleDataIntegrityViolationException(
                new DataIntegrityViolationException("insert into sys_user ..."), mock(WebRequest.class));

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertEquals("数据完整性校验失败", response.getBody().getDescription());
    }
}
