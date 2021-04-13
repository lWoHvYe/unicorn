package com.lwohvye.utils.mapper;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.ObjectUtil;

import java.sql.Timestamp;

/**
 * @author Hongyan Wang
 * @description Timestamp&String
 * @date 2021/4/13 5:07 下午
 */
public class TimestampMapper {

    public String asString(Timestamp timestamp) {
        return ObjectUtil.isNotNull(timestamp) ? LocalDateTimeUtil.format(timestamp.toLocalDateTime(), DatePattern.NORM_DATETIME_FORMATTER) : null;
    }

    public Timestamp asTimestamp(String str) {
        return ObjectUtil.isNotNull(str) ? Timestamp.valueOf(str) : null;
    }
}
