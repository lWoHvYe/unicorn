package com.lwohvye;

import cn.hutool.core.util.IdUtil;

/**
 * @author Hongyan Wang
 * @date 2021年04月10日 10:34
 */
public record Friend(String name, Integer age) {
    static Long uuid = 1L;
}
