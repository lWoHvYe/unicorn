package com.lwohvye.utils.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @author liangming.deng
 * @description jackson日期转换工具类
 * 这里似乎影响了json的时间格式化，包括http返回的
 */
public class JsonDateFormatFull extends JsonSerializer<Date> {

    /**
     * Jackson支持日期字符串格式。这部分应该可以直接在配置文件里配置
     * "yyyy-MM-dd'T'HH:mm:ss.SSSZ" "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
     * "EEE, dd MMM yyyy HH:mm:ss zzz" "yyyy-MM-dd"
     */
    @Override
    public void serialize(Date value, JsonGenerator jgen, SerializerProvider provider) throws IOException {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String formattedDate = formatter.format(value);
        jgen.writeString(formattedDate);
    }
}
