/*
 *    Copyright (c) 2021-2023.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.core.utils;

import com.lwohvye.core.annotation.Blob2String;
import org.springframework.stereotype.Component;

/**
 * 需注意要与使用方在同一模块内。原因未知
 * 已知是，当放在common模块中时，在system模块使用会报找不到方法的错误
 *
 * @author Hongyan Wang
 * @date 2021/4/1 0:05
 */
@Component
public class ConvertBlob2StringUtil {

    @Blob2String
    public String convert(String in) {
        return StringUtils.convertToString(in);
    }
}
