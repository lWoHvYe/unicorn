/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.core.utils.mapper;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.ObjectUtil;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * transform DateTime to String
 *
 * @author Hongyan Wang
 * @date 2021/4/13 5:06 下午
 */
@Component
public class DateTimeMapper {

    public String asString(Date date) {
        return ObjectUtil.isNotNull(date) ? DateUtil.format(date, DatePattern.NORM_DATETIME_FORMAT) : null;
    }

    public Date asDate(String str) {
        return ObjectUtil.isNotNull(str) ? DateUtil.parse(str, DatePattern.NORM_DATETIME_FORMAT) : null;
    }
}
