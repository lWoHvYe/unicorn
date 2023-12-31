/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.beans.mapper;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.date.Week;
import cn.hutool.core.util.ObjectUtil;
import org.springframework.stereotype.Component;

import java.sql.Timestamp;

/**
 * transform Timestamp to String。使用方式为加入到Mapper的uses中
 * 此类方法，mapstruct自动选择的依据是入参和出参，在方法内要保持唯一
 *
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
     * 指定转换格式。静态
     * target为 dto中的属性。方法需为静态方法，且要指定好入参
     *
     * @param timestamp
     * @param format
     * @return java.lang.String
     * @Mappings({
     * @Mapping(target = "date", expression = "java(com.lwohvye.utils.mapper.TimestampMapper.format2String( entity.getStartDatetime(), \"yyyy-MM-dd\" ))")
     * })
     * @date 2021/4/14 10:12 上午
     */
    public static String format2String(Timestamp timestamp, String format) {
        return ObjectUtil.isNotNull(timestamp) ? LocalDateTimeUtil.format(timestamp.toLocalDateTime(), format) : null;
    }

    /**
     * 获取时星期几
     *
     * @param timestamp
     * @return cn.hutool.core.date.Week
     * @date 2021/4/14 10:55 上午
     */
    public static Week format2Week(Timestamp timestamp) {
        return ObjectUtil.isNotNull(timestamp) ? DateUtil.dayOfWeekEnum(timestamp) : null;
    }

}
