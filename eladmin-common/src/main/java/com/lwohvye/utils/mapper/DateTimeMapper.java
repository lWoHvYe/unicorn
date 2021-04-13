package com.lwohvye.utils.mapper;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.ObjectUtil;

import java.util.Date;

/**
 * @author Hongyan Wang
 * @description DateTime&String
 * @date 2021/4/13 5:06 下午
 */
public class DateTimeMapper {

    public String asString(Date date) {
        return ObjectUtil.isNotNull(date) ? DateUtil.format(date, DatePattern.NORM_DATETIME_FORMAT) : null;
    }

    public Date asDate(String str) {
        return ObjectUtil.isNotNull(str) ? DateUtil.parse(str, DatePattern.NORM_DATETIME_FORMAT) : null;
    }
}
