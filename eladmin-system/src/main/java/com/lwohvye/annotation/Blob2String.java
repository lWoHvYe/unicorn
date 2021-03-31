package com.lwohvye.annotation;

import org.mapstruct.Qualifier;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @description 针对String取blob类型乱码问题
 * @author Hongyan Wang
 * @date 2021/3/15 8:16 下午
 */
@Qualifier
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.SOURCE)
public @interface Blob2String {
}
