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
package com.lwohvye.utils.serializer;

import cn.hutool.core.lang.Assert;
import com.alibaba.fastjson.JSON;
import com.lwohvye.utils.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.springframework.data.redis.serializer.RedisSerializer;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * 重写序列化器
 *
 * @author /
 */
public class StringRedisSerializer implements RedisSerializer<Object> {

    private final Charset charset;

    public StringRedisSerializer() {
        this(StandardCharsets.UTF_8);
    }

    private StringRedisSerializer(Charset charset) {
        Assert.notNull(charset, "Charset must not be null!");
        this.charset = charset;
    }

    @Override
    public String deserialize(byte[] bytes) {
        return (bytes == null ? null : new String(bytes, charset));
    }

    @Override
    public byte[] serialize(Object object) {
        var string = "";
        // 本来就是String类型的话，就不转json了。
        if (object instanceof String str)
            string = str;
        else
            // 非String类型的key比较少。
            string = JSON.toJSONString(object);
        if (StringUtils.isBlank(string))
            return new byte[0];
//        string = string.replace("\"", "");
        // 反序列化
        StringEscapeUtils.unescapeJava(string);
        return string.getBytes(charset);
    }
}
