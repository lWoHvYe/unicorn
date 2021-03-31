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
