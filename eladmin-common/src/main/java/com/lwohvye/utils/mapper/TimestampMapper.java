package com.lwohvye.utils.mapper;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.date.Week;
import cn.hutool.core.util.ObjectUtil;
import org.springframework.stereotype.Component;

import java.sql.Timestamp;

/**
 * @author Hongyan Wang
 * @description Timestamp&String。使用方式为加入到Mapper的uses中
 *  此类方法，mapstruct自动选择的依据是入参和出参，在方法内要保持唯一
 * @date 2021/4/13 5:07 下午
 */
@Component
public class TimestampMapper {

    public String asString(Timestamp timestamp) {
        return ObjectUtil.isNotNull(timestamp) ? LocalDateTimeUtil.format(timestamp.toLocalDateTime(), DatePattern.NORM_DATETIME_FORMATTER) : null;
    }

    public Timestamp asTimestamp(String str) {
        return ObjectUtil.isNotNull(str) ? Timestamp.valueOf(str) : null;
    }

    /**
     * @param timestamp
     * @param format
     * @return java.lang.String
     * @description 指定转换格式。静态
     * target为 dto中的属性。方法需为静态方法，且要指定好入参
     * @Mappings({
     * @Mapping(target = "date", expression = "java(com.lwohvye.utils.mapper.TimestampMapper.format2String( entity.getStartDatetime(), \"yyyy-MM-dd\" ))")
     * })
     * @author Hongyan Wang
     * @date 2021/4/14 10:12 上午
     */
    public static String format2String(Timestamp timestamp, String format) {
        return ObjectUtil.isNotNull(timestamp) ? LocalDateTimeUtil.format(timestamp.toLocalDateTime(), format) : null;
    }

    /**
     * @param timestamp
     * @return cn.hutool.core.date.Week
     * @description 获取时星期几
     * @author Hongyan Wang
     * @date 2021/4/14 10:55 上午
     */
    public static Week format2Week(Timestamp timestamp) {
        return ObjectUtil.isNotNull(timestamp) ? DateUtil.dayOfWeekEnum(timestamp) : null;
    }

}
