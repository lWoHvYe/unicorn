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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * @author Hongyan Wang
 * @description Json工具类
 * @date 2021/11/10 11:42 下午
 */
@Slf4j
public class JsonUtils {
    // 加载速度太慢了，放在静态代码块中
    // private static final ObjectMapper mapper = new ObjectMapper();
    private static ObjectMapper objectMapper;

    /**
     * 设置一些通用的属性
     */
    static {
        objectMapper = new ObjectMapper();
        // 如果json中有新增的字段并且是实体类类中不存在的，不报错
        // mapper.configure(DeserializationFeature.FAIL_ON_IGNORED_PROPERTIES, false);
        // 如果存在未知属性，则忽略不报错，允许pojo中有在json串中不存在的字段
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        // 允许key没有双引号
        objectMapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
        // 允许key有单引号
        objectMapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
        // 允许整数以0开头
//        objectMapper.configure(JsonParser.Feature.ALLOW_NUMERIC_LEADING_ZEROS, true);
        // 允许字符串中存在回车换行控制符
//        objectMapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_CONTROL_CHARS, true);
        // 允许有注释
        objectMapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
    }

    // region   toJSONString

    public static String toJSONString(Object obj) {
        return toJSONString(obj, () -> "", false);
    }

    public static String toFormatJSONString(Object obj) {
        return toJSONString(obj, () -> "", true);
    }

    /**
     * @param obj
     * @param defaultSupplier
     * @param format
     * @return java.lang.String
     * @description toJSONString 底层实现。简单类型直接返回
     * @date 2021/11/10 9:37 下午
     */
    public static String toJSONString(Object obj, Supplier<String> defaultSupplier, boolean format) {
        try {
            if (Objects.isNull(obj))
                return defaultSupplier.get();

            if (obj instanceof String str)
                return str;

            if (obj instanceof Number)
                return obj.toString();

            if (format)
                return objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(obj);

            return objectMapper.writeValueAsString(obj);
        } catch (Exception e) {
            log.error(String.format("toJSONString %s", !Objects.isNull(obj) ? obj.toString() : "null"), e);
        }
        return defaultSupplier.get();
    }

    // endregion

    // region   toJavaObject
    public static <T> T toJavaObject(Object obj, Class<T> tClass) {
        return toJavaObject(obj, tClass, () -> null);
    }

    public static <T> T toJavaObject(InputStream inputStream, Class<T> tClass) {
        Reader reader = new InputStreamReader(inputStream);
        try {
            return objectMapper.readValue(reader, tClass);
        } catch (IOException e) {
            log.error(String.format("toJavaObject exception: %n %s%n %s", inputStream.getClass().getSimpleName(), tClass), e);
        }
        return null;
    }

    public static <T> T toJavaObject(String value, TypeReference<T> valueTypeRef) {
        try {
            return objectMapper.readValue(value, valueTypeRef);
        } catch (IOException e) {
            log.error(String.format("toJavaObject exception: %n %s%n %s", value, valueTypeRef), e);
        }
        return null;
    }

    /**
     * @param obj
     * @param tClass
     * @param defaultSupplier
     * @return T
     * @description toJavaObject底层实现
     * @date 2021/11/10 9:39 下午
     */
    public static <T> T toJavaObject(Object obj, Class<T> tClass, Supplier<T> defaultSupplier) {
        try {
            if (Objects.isNull(obj))
                return defaultSupplier.get();

            var str = toJSONString(obj);
            return objectMapper.readValue(str, tClass);
        } catch (Exception e) {
            log.error(String.format("toJavaObject exception: %n %s%n %s", obj, tClass), e);
        }
        return defaultSupplier.get();
    }

    // endregion

    // region   toJavaObjectList

    public static <T> List<T> toJavaObjectList(Object obj, Class<T> tClass) {
        return toJavaObjectList(obj, tClass, Collections::emptyList);
    }

    /**
     * @param obj
     * @param tClass
     * @param defaultSupplier
     * @return java.util.List<T>
     * @description toJavaObjectList底层实现
     * @date 2021/11/10 9:45 下午
     */
    public static <T> List<T> toJavaObjectList(Object obj, Class<T> tClass, Supplier<List<T>> defaultSupplier) {
        try {
            if (Objects.isNull(obj))
                return defaultSupplier.get();

            var str = toJSONString(obj);
            JavaType javaType = objectMapper.getTypeFactory().constructParametricType(List.class, tClass);
            return objectMapper.readValue(str, javaType);
        } catch (Exception e) {
            log.error(String.format("toJavaObjectList exception %n%s%n%s", obj, tClass), e);
        }
        return defaultSupplier.get();
    }

    // endregion

    // 简单地直接用json复制或者转换(Cloneable)
    public static <T> T jsonCopy(Object obj, Class<T> tClass) {
        return obj != null ? toJavaObject(obj, tClass) : null;
    }

    // region   toCollection

    public static Map<String, Object> toMap(Object obj) {
        return toT(obj, Map.class, Collections::emptyMap);
    }

    public static List toList(Object obj) {
        return toT(obj, List.class, Collections::emptyList);
    }

    /**
     * @param obj
     * @param tClass
     * @param defaultSuppler
     * @return T
     * @description
     * @date 2021/11/11 12:30 上午
     */
    public static <T> T toT(Object obj, Class<T> tClass, Supplier<T> defaultSuppler) {
        try {
            if (Objects.isNull(obj))
                return defaultSuppler.get();

            if (tClass.isInstance(obj))
                return tClass.cast(obj);

            return toJavaObject(obj, tClass, defaultSuppler);
        } catch (Exception e) {
            log.error(String.format("toEntity-T exception %n%s %n%s", tClass.getSimpleName(), obj), e);
        }
        return defaultSuppler.get();
    }

    // endregion

    // region 从map中获取指定类型的数据

    public static String getString(Map<String, Object> map, String key) {
        if (map.isEmpty())
            return "";

        var valueStr = String.valueOf(map.get(key));
        return StringUtils.isNotEmpty(valueStr) ? valueStr : "";
    }

    public static long getLong(Map<String, Object> map, String key) {
        if (map.isEmpty())
            return 0L;

        var valueStr = String.valueOf(map.get(key));
        if (StringUtils.isBlank(valueStr) || !StringUtils.isNumeric(valueStr))
            return 0L;
        return Long.parseLong(valueStr);
    }

    public static int getInt(Map<String, Object> map, String key) {
        if (map.isEmpty())
            return 0;

        var valueStr = String.valueOf(map.get(key));
        if (StringUtils.isBlank(valueStr) || !StringUtils.isNumeric(valueStr))
            return 0;
        return Integer.parseInt(valueStr);
    }

    // endregion
}


