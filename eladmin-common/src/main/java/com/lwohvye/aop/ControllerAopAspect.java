package com.lwohvye.aop;

import com.lwohvye.exception.BadRequestException;
import com.lwohvye.utils.ResultModel;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;
import org.springframework.jdbc.BadSqlGrammarException;
import org.springframework.stereotype.Component;

/**
 * @author Hongyan Wang
 * @packageName com.lwohvye.aop
 * @className ControllerAopAspect
 * @description 用于捕获Controller层异常，转化为失败信息，与{@link ResultModel}配合使用
 * AOP方案也只是做异常处理。可以使用全局异常处理的方式替换。参照这个https://www.lwohvye.com/2021/01/07/736/
 * 这个可以借鉴的是，通过以某个类型为切点，可以在方法执行前后做些事情。不同于日志
 * @date 2020/1/14 13:45
 * @see com.lwohvye.exception.handler.ApiGlobalExceptionHandler 新的统一异常处理
 * @see com.lwohvye.utils.result.ResultInfo 统一结果返回
 */
// ControllerAopAspect和ResultModel可能需要重做。使用统一异常处理+ResultInfo的形式。更加通用。该方法已不使用
@Order(2)
@Component
@Aspect
@Slf4j
public class ControllerAopAspect {

    /**
     * @description 定义切点，使用指定实体类ResultModel为返回值的public方法作为切点
     * @date 2020/1/14 14:24
     */
    @Pointcut("execution(public com.lwohvye.*.ResultModel *(..))")
    public void handlerControllerPointcut() {
    }

    @Around("handlerControllerPointcut()")
    public Object handlerControllerMethod(ProceedingJoinPoint pjp) {
        ResultModel<?> result;
        try {
//            获取程序执行结果
            result = (ResultModel<?>) pjp.proceed();
        } catch (Throwable throwable) {
            result = handlerControllerException(pjp, throwable);
        }
        return result;
    }

    private ResultModel<?> handlerControllerException(ProceedingJoinPoint pjp, Throwable throwable) {
        ResultModel<?> result = new ResultModel<>(throwable);
//        这里可以根据不同的异常，做出不同的操作
//        sql语法错误
        if (throwable instanceof BadSqlGrammarException) {
            result.setMsg(ResultModel.SQL_ERROR_MSG);
            result.setCode(ResultModel.SQL_ERROR_CODE);
        }
//        用户权限不足
//        if (throwable instanceof UnauthorizedException) {
//            result.setMsg(ResultModel.NEED_PERMISSION_MSG);
//            result.setCode(ResultModel.NEED_PERMISSION);
//        }

        if (throwable instanceof BadRequestException) {
            result.setMsg(ResultModel.BAD_REQUEST_MSG);
            result.setCode(ResultModel.BAD_REQUEST);
        }
        return result;
    }
}
