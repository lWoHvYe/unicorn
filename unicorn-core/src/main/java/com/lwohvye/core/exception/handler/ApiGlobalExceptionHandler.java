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
package com.lwohvye.core.exception.handler;

import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.exception.EntityExistException;
import com.lwohvye.core.exception.EntityNotFoundException;
import com.lwohvye.core.utils.result.ResultInfo;
import com.lwohvye.core.utils.ThrowableUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Objects;


/**
 * 全局异常处理，
 * 统一异常处理，因为已在SpringSecurityConfig里配置了401和403的handler，所以鉴权中的，401和403异常无法处理，需⚠️。但@el.check中但403可以被处理
 * Created by cy on 2021/01/08.
 */
@RestControllerAdvice
// 使用Order使其先于GlobalExceptionHandler执行，这里把优先级设置为了最高。原统一异常处理可移除
@Order(Ordered.HIGHEST_PRECEDENCE) // 移除GlobalExceptionHandler后，可能需要调整下Order
@Slf4j
@ConditionalOnWebApplication // 在Spring为Web服务时生效
@ConditionalOnMissingBean(ApiGlobalExceptionHandler.class) // 避免Bean冲突
public class ApiGlobalExceptionHandler {

    /**
     * 处理所有未知异常 Throwable
     *
     * @param e /
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(Throwable.class)
    public ResponseEntity<ResultInfo> handleException(Throwable e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.failed(e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    /**
     * 部分非受检异常，统一处理
     *
     * @param e /
     * @return ResponseEntity/
     * @date 2022/2/24 10:59 AM
     */
    @ResponseBody
    @ExceptionHandler({
            EntityExistException.class // 实体已存在
            , EntityNotFoundException.class // 实体不存在
            , BadRequestException.class // 自定义异常
            , IllegalStateException.class // Assert相关
            , IllegalArgumentException.class // Assert相关
    })
    public ResponseEntity<ResultInfo> handleServletException(RuntimeException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.failed(e.getMessage()), HttpStatus.BAD_REQUEST);
    }

    /**
     * 处理无权限访问异常 AccessDeniedException 403
     *
     * @param e /
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ResultInfo> handleAccessDeniedException(AccessDeniedException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.forbidden(e.getMessage()), HttpStatus.FORBIDDEN);
    }

    /**
     * 处理请求方法不支持的异常 HttpRequestMethodNotSupportedException
     *
     * @param e /
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(value = HttpRequestMethodNotSupportedException.class)
    public ResponseEntity<ResultInfo> handleHttpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.methodNotAllowed(e.getMessage()), HttpStatus.METHOD_NOT_ALLOWED);
    }

    /**
     * 处理请求参数不正确的异常 HttpRequestMethodNotSupportedException
     *
     * @param e /
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ResultInfo> handleMethodArgumentNotValidException(MethodArgumentNotValidException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        String[] str = Objects.requireNonNull(e.getBindingResult().getAllErrors().get(0).getCodes())[1].split("\\.");
        String message = e.getBindingResult().getAllErrors().get(0).getDefaultMessage();
        if ("不能为空".equals(message)) {
            message = str[1] + ":" + message;
        }
        return buildResponseEntity(ResultInfo.validateFailed(message), HttpStatus.BAD_REQUEST);
    }

    /**
     * 添加或更新的数据中有非空字段设置为null。或者未使用级联删除外键，均会出此异常
     *
     * @return org.springframework.http.ResponseEntity
     * @params e /
     * @date 2021/1/9 21:42
     */
    @ResponseBody
    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ResultInfo> handleDataIntegrityViolationException(DataIntegrityViolationException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.methodNotAllowed(e.getMessage()), HttpStatus.BAD_REQUEST);
    }

    private ResponseEntity<ResultInfo> buildResponseEntity(ResultInfo resultInfo, HttpStatus httpStatus) {
        return new ResponseEntity<>(resultInfo, httpStatus);
    }
}
