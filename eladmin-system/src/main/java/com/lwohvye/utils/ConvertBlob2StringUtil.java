/*
 *  Copyright 2020-2022 lWoHvYe
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.utils;

import com.lwohvye.annotation.Blob2String;
import org.springframework.stereotype.Component;

/**
 * @author Hongyan Wang
 * @description 需注意要与使用方在同一模块内。原因未知
 * 已知是，当放在common模块中时，在system模块使用会报找不到方法的错误
 * @date 2021/4/1 0:05
 */
@Component
public class ConvertBlob2StringUtil {

    @Blob2String
    public String convert(String in) {
        return StringUtils.convertToString(in);
    }
}
