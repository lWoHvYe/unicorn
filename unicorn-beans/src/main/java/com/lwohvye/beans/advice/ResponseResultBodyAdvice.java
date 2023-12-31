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
package com.lwohvye.beans.advice;

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.ThrowableUtils;
import com.lwohvye.core.utils.result.ResultInfo;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.Nullable;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.FieldError;
import org.springframework.validation.ObjectError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;
import org.springframework.web.util.WebUtils;

import jakarta.persistence.EntityExistsException;
import jakarta.persistence.EntityNotFoundException;
import jakarta.validation.ConstraintViolationException;

import java.lang.annotation.Annotation;
import java.util.Objects;

/**
 * 这也是一种统一数据返回的方式。可视情况整合。与{@link RespResultBody}配合使用
 * ResponseBodyAdvice 实现了这个接口的类，处理返回的值在传递给 HttpMessageConverter之前。应用场景为spring项目开发过程中，对controller层返回值进行修改增强处理（比如加密、统一返回格式等）。
 * 另外还有RequestBodyAdvice用于在请求之前进行一些操作
 *
 * @date 2021/11/10 12:42 下午
 */
@Slf4j
// @ConditionalOnClass(name = "org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice") // 这种比较适合目标类在上层，无法直接引用的情况，毕竟类名有些长
@ConditionalOnClass(ResponseBodyAdvice.class) // 这个跟上面那种是等价的，Conditional系列不会报ClassNotFound的Exception。这个配置用于实现当exclude WebMVC使用 WebFlux时，不会init该Bean，也不会报错
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET) // 在Spring为Web服务时生效
@ControllerAdvice // 这里用@RestControllerAdvice与@ControllerAdvice好像没区别，本质也是处理链，这只是上面的一环
public class ResponseResultBodyAdvice implements ResponseBodyAdvice<Object> {

    private static final Class<? extends Annotation> ANNOTATION_TYPE = RespResultBody.class;

    /**
     * 判断类或者方法是否使用了 @ResponseResultBody
     */
    @Override
    public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) { // 此时已构建了 ResponseEntity
        return AnnotatedElementUtils.hasAnnotation(returnType.getContainingClass(), ANNOTATION_TYPE) || returnType.hasMethodAnnotation(ANNOTATION_TYPE);
    }

    /**
     * 当类或者方法使用了 @ResponseResultBody 就会调用这个方法，尽量不要直接返回String，别的应该都可以
     * 针对status是200的可以只返回body，否则可以返回ResponseEntity，这里只改body部分，所以都支持
     */
    // 如方法所言，这里设置的是Body，所以是无法改变ResponseStatus的（虽然一般不用管），另外针对String类型的处理还有些问题
    @Override
    public Object beforeBodyWrite(@Nullable Object body,
                                  MethodParameter returnType,
                                  MediaType selectedContentType,
                                  Class<? extends HttpMessageConverter<?>> selectedConverterType,
                                  ServerHttpRequest request,
                                  ServerHttpResponse response) {
        if (Objects.isNull(body)) {
            var parameterType = returnType.getParameterType(); // 或者GenericParameterType
            if (Objects.equals(String.class, parameterType)) // String类型会转成空字符串
                return "";
            return ResultInfo.success(); // 别的都转成标准格式

        }
        if (body instanceof ResultInfo<?> resInfo) // 如果返回值是标准格式，就不需要再次封装，如果不加该判断的话，异常的结果会被封装两次
            return resInfo;
        if (body instanceof String str) // String 类型很特殊，不能直接用ResultInfo.success，最后会将其转为String类型，然后出现ClassCastException:
            // class com.lwohvye.core.utils.result.ResultInfo cannot be cast to class java.lang.String
            // 主体是因为selectedContentType和selectedConverterType的差异（这个是与方法的returnType有关），String时是: text/html 和 class org.springframework.http.converter.StringHttpMessageConverter，
            // 其他类型时是："application/json" 和 "class org.springframework.http.converter.json.MappingJackson2HttpMessageConverter"
            return ResultInfo.success(str).toString();
        return ResultInfo.success(body);
    }

// region 统一异常处理

    /**
     * 提供对标准Spring MVC异常的处理
     *
     * @param ex      the target exception
     * @param request the current request
     */
    @ExceptionHandler(Exception.class)
    public final ResponseEntity<ResultInfo<?>> handleException(Exception ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        ResultInfo<?> body = ResultInfo.failed(ex.getMessage());
        var headers = new HttpHeaders();
        HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * 部分非受检异常，统一处理
     *
     * @param ex /
     * @return ResponseEntity/
     * @date 2022/2/24 10:59 AM
     */
    @ExceptionHandler({
            EntityExistsException.class // 实体已存在
            , EntityNotFoundException.class // 实体不存在
            , BadRequestException.class // 自定义异常
            , IllegalStateException.class // Assert相关
            , IllegalArgumentException.class // Assert相关
    })
    public final ResponseEntity<ResultInfo<?>> handleServletException(RuntimeException ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        var body = ResultInfo.failed(ex.getMessage());
        var headers = new HttpHeaders();
        var status = HttpStatus.BAD_REQUEST;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * 处理无权限访问异常 AccessDeniedException 403
     * 401和403有配置处理，故不会走到这边来
     *
     * @param ex /
     * @return ResponseEntity
     */
    @ExceptionHandler(AccessDeniedException.class)
    public final ResponseEntity<ResultInfo<?>> handleAccessDeniedException(AccessDeniedException ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        var body = ResultInfo.forbidden(ex.getMessage());
        var headers = new HttpHeaders();
        var status = HttpStatus.FORBIDDEN;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * 处理请求方法不支持的异常 HttpRequestMethodNotSupportedException
     *
     * @param ex /
     * @return ResponseEntity
     */
    @ExceptionHandler(value = HttpRequestMethodNotSupportedException.class)
    public final ResponseEntity<ResultInfo<?>> handleHttpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        var body = ResultInfo.methodNotAllowed(ex.getMessage());
        var headers = new HttpHeaders();
        var status = HttpStatus.METHOD_NOT_ALLOWED;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * {@code @PathVariable} 和 {@code @RequestParam} 参数校验不通过时抛出的异常处理
     */
    @ExceptionHandler({ConstraintViolationException.class})
    public final ResponseEntity<ResultInfo<?>> handleConstraintViolationException(ConstraintViolationException ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        var body = ResultInfo.validateFailed(ex.getMessage());
        var headers = new HttpHeaders();
        var status = HttpStatus.BAD_REQUEST;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * 处理请求参数不正确的异常 HttpRequestMethodNotSupportedException
     * {@code @RequestBody} 参数校验不通过时抛出的异常处理
     *
     * @param ex /
     * @return ResponseEntity
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public final ResponseEntity<ResultInfo<?>> handleMethodArgumentNotValidException(MethodArgumentNotValidException ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        ObjectError objectError = ex.getBindingResult().getAllErrors().get(0);
        String message = objectError.getDefaultMessage();
        if (objectError instanceof FieldError error) {
            message = error.getField() + ": " + message;
        }
        var body = ResultInfo.validateFailed(message);
        var headers = new HttpHeaders();
        var status = HttpStatus.BAD_REQUEST;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }

    /**
     * 添加或更新的数据中有非空字段设置为null。或者未使用级联删除外键，均会出此异常
     *
     * @return org.springframework.http.ResponseEntity
     * @params e /
     * @date 2021/1/9 21:42
     */
    @ExceptionHandler(DataIntegrityViolationException.class)
    public final ResponseEntity<ResultInfo<?>> handleDataIntegrityViolationException(DataIntegrityViolationException ex, WebRequest request) {
        log.error(ThrowableUtils.getStackTrace(ex));
        var body = ResultInfo.methodNotAllowed(ex.getMessage());
        var headers = new HttpHeaders();
        var status = HttpStatus.BAD_REQUEST;
        return this.handleExceptionInternal(ex, body, headers, status, request);
    }
// endregion

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
            request.setAttribute(WebUtils.ERROR_EXCEPTION_ATTRIBUTE, ex, RequestAttributes.SCOPE_REQUEST);
        }
        return new ResponseEntity<>(body, headers, status);
    }
}
