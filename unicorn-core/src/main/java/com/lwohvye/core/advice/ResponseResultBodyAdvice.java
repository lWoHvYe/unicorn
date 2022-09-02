/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.core.advice;

import com.lwohvye.core.annotation.ResponseResultBody;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.result.ResultInfo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;
import org.springframework.web.util.WebUtils;

import java.lang.annotation.Annotation;

/**
 * 这也是一种统一数据返回的方式。可视情况整合。与{@link ResponseResultBody}配合使用
 * ResponseBodyAdvice 实现了这个接口的类，处理返回的值在传递给 HttpMessageConverter之前。应用场景为spring项目开发过程中，对controller层返回值进行修改增强处理（比如加密、统一返回格式等）。
 *
 * @date 2021/11/10 12:42 下午
 */
@Slf4j
// @ConditionalOnClass(name = "org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice") // 这种比较适合目标类在上层，无法直接引用的情况，毕竟类名有些长
@ConditionalOnClass(ResponseBodyAdvice.class) // 这个跟上面那种是等价的，Conditional系列不会报ClassNotFound的Exception。这个配置用于实现当exclude WebMVC使用 WebFlux时，不会init该Bean，也不会报错
@RestControllerAdvice
public class ResponseResultBodyAdvice implements ResponseBodyAdvice<Object> {

    private static final Class<? extends Annotation> ANNOTATION_TYPE = ResponseResultBody.class;

    /**
     * 判断类或者方法是否使用了 @ResponseResultBody
     */
    @Override
    public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) { // 此时已构建了 ResponseEntity
        return AnnotatedElementUtils.hasAnnotation(returnType.getContainingClass(), ANNOTATION_TYPE) || returnType.hasMethodAnnotation(ANNOTATION_TYPE);
    }

    /**
     * 当类或者方法使用了 @ResponseResultBody 就会调用这个方法
     */
    // TODO: 2021/2/4 当前已将数据返回都使用ResultInfo.success的方式。可以着手整合这种方式了，但原来返回的是 ResponseEntity。处理方式待定
    @Override
    public Object beforeBodyWrite(Object body, MethodParameter returnType, MediaType selectedContentType, Class<? extends HttpMessageConverter<?>> selectedConverterType, ServerHttpRequest request, ServerHttpResponse response) {
        if (body instanceof ResultInfo) {
            return body;
        }
        return ResultInfo.success(body);
    }


    /**
     * 提供对标准Spring MVC异常的处理
     *
     * @param ex      the target exception
     * @param request the current request
     */
    @ExceptionHandler(Exception.class)
    public final ResponseEntity<ResultInfo<?>> exceptionHandler(Exception ex, WebRequest request) {
        log.error("ExceptionHandler: {}", ex.getMessage());
        HttpHeaders headers = new HttpHeaders();
        if (ex instanceof BadRequestException e) {
            return this.handleResultException(e, headers, request);
        }
        return this.handleException(ex, headers, request);
    }

    /**
     * 对ResultException类返回返回结果的处理
     */
    protected ResponseEntity<ResultInfo<?>> handleResultException(BadRequestException ex, HttpHeaders headers, WebRequest request) {
        ResultInfo<?> body = ResultInfo.failed(ex.getMessage());
        HttpStatus status = HttpStatus.BAD_REQUEST;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * 异常类的统一处理
     */
    protected ResponseEntity<ResultInfo<?>> handleException(Exception ex, HttpHeaders headers, WebRequest request) {
        ResultInfo<?> body = ResultInfo.failed("");
        HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler#handleExceptionInternal(java.lang.Exception, java.lang.Object, org.springframework.http.HttpHeaders, org.springframework.http.HttpStatus, org.springframework.web.context.request.WebRequest)
     * <p>
     * A single place to customize the response body of all exception types.
     * <p>The default implementation sets the {@link WebUtils#ERROR_EXCEPTION_ATTRIBUTE}
     * request attribute and creates a {@link ResponseEntity} from the given
     * body, headers, and status.
     */
    protected ResponseEntity<ResultInfo<?>> handleExceptionInternal(Exception ex, ResultInfo<?> body, HttpHeaders headers, HttpStatus status, WebRequest request) {

        if (HttpStatus.INTERNAL_SERVER_ERROR.equals(status)) {
            request.setAttribute(WebUtils.ERROR_EXCEPTION_ATTRIBUTE, ex, WebRequest.SCOPE_REQUEST);
        }
        return new ResponseEntity<>(body, headers, status);
    }
}
