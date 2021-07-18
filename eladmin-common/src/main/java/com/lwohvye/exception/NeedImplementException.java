package com.lwohvye.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;

import static org.springframework.http.HttpStatus.NOT_IMPLEMENTED;

/**
 * @author Hongyan Wang
 * @date 2021年07月18日 18:54
 * @description 不能使用默认实现 ，需对方法默认实现
 */
@Getter
public class NeedImplementException extends RuntimeException {
    private Integer status = NOT_IMPLEMENTED.value();

    public NeedImplementException(String msg) {
        super(msg);
    }

    public NeedImplementException(HttpStatus status, String msg) {
        super(msg);
        this.status = status.value();
    }
}
