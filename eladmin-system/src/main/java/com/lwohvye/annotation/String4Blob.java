package com.lwohvye.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @description 标识使用String来存取Blob类型的数据
 * @author Hongyan Wang
 * @date 2021年03月31日 21:42
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface String4Blob {
}
