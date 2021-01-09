package com.lwohvye.exception.handler;

import com.lwohvye.exception.BadRequestException;
import com.lwohvye.exception.EntityExistException;
import com.lwohvye.exception.EntityNotFoundException;
import com.lwohvye.utils.ThrowableUtil;
import com.lwohvye.utils.result.ResultInfo;
import lombok.extern.slf4j.Slf4j;
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
 * 全局异常处理
 * Created by cy on 2021/01/08.
 */
@RestControllerAdvice
// 使用Order使其先于GlobalExceptionHandler执行。原统一异常处理可移除
@Order(Ordered.HIGHEST_PRECEDENCE)
@Slf4j
public class ApiGlobalExceptionHandler {

    /**
     * 处理所有未知异常 Throwable
     *
     * @param e
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(Throwable.class)
    public ResponseEntity<ResultInfo> handleException(Throwable e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.failed(e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    /**
     * 处理实体已存在异常 EntityExistException
     *
     * @param e
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(value = EntityExistException.class)
    public ResponseEntity<ResultInfo> entityExistException(EntityExistException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.failed(e.getMessage()), HttpStatus.BAD_REQUEST);
    }

    /**
     * 处理实体不存在异常 EntityNotFoundException
     *
     * @param e
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(value = EntityNotFoundException.class)
    public ResponseEntity<ResultInfo> entityNotFoundException(EntityNotFoundException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.failed(e.getMessage()), HttpStatus.BAD_REQUEST);
    }

    /**
     * 处理自定义异常 BadRequestException
     *
     * @param e
     * @return ResponseEntity
     */
    @ResponseBody
    @ExceptionHandler(value = BadRequestException.class)
    public ResponseEntity<ResultInfo> badRequestException(BadRequestException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.failed(e.getMessage()), HttpStatus.BAD_REQUEST);
    }

    /**
     * 处理无权限访问异常 AccessDeniedException
     *
     * @param e
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
     * @param e
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
     * @param e
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
     * @description 添加或更新的数据中有非空字段设置为null。或者未使用级联删除外键，均会出此异常
     * @params [e]
     * @return org.springframework.http.ResponseEntity<com.lwohvye.utils.result.ResultInfo>
     * @author Hongyan Wang
     * @date 2021/1/9 21:42
     */
    @ResponseBody
    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ResultInfo> handleDataIntegrityViolationException(DataIntegrityViolationException e) {
        log.error(ThrowableUtil.getStackTrace(e));
        return buildResponseEntity(ResultInfo.methodNotAllowed(e.getMessage()), HttpStatus.UNPROCESSABLE_ENTITY);
    }

    private ResponseEntity<ResultInfo> buildResponseEntity(ResultInfo resultInfo, HttpStatus httpStatus) {
        return new ResponseEntity<>(resultInfo, httpStatus);
    }
}
