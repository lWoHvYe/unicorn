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

package com.lwohvye.core.utils.result;

import com.lwohvye.core.exception.BadRequestException;
import lombok.SneakyThrows;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import jakarta.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 主动返回数据
 *
 * @date 2021/11/15 7:19 下午
 */
@SuppressWarnings("unchecked")
public class ResultUtils {


    @SneakyThrows
    public static void resultJson(@NotNull HttpServletResponse response, int responseStatus, String msg) {
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setStatus(responseStatus);
        try (PrintWriter out = response.getWriter()) {
            out.append(msg);
        }
    }

    /**
     * 从Restful调用的结果中，获取目标值，简单类型
     *
     * @param responseEntity Restful返回的结果，统一返回形式
     * @param tClass         目标类型
     * @return T             目标值
     * @date 2022/2/27 11:10 PM
     */
    @Nullable // 放在参数上，表示参数可为null，放在属性上，表示属性可为null，放在方法上，表示方法可能返回null
    public static <T> T getEntityFromResp(ResponseEntity<?> responseEntity, Class<T> tClass) {
        checkResp(responseEntity);
        return responseEntity.getBody() instanceof ResultInfo<?> resultInfo && tClass.isInstance(resultInfo.getResult())
                ? tClass.cast(resultInfo.getResult()) : null;
    }

    @Nullable // 放在参数上，表示参数可为null，放在属性上，表示属性可为null，放在方法上，表示方法可能返回null
    public static <T> T getEntityFromResp(ResponseEntity<ResultInfo<T>> responseEntity) {
        checkResp(responseEntity);
        return Optional.ofNullable(responseEntity.getBody()).orElse(ResultInfo.success()).getResult();
    }

    /**
     * 从Restful调用的结果中，获取目标集合。若返回的是分页结果集，则只返回结果集部分
     *
     * @param responseEntity Restful返回的结果
     * @param tClass         集合中数据类型，参与用于定义类型
     * @return java.util.List<T> 目标集合
     * @date 2022/2/27 11:25 PM
     */
    public static <T> List<T> getListFromResp(ResponseEntity<?> responseEntity, Class<T> tClass) {
        checkResp(responseEntity);
        return responseEntity.getBody() instanceof ResultInfo<?> resultInfo ? (List<T>) resultInfo.getContent() : Collections.emptyList();
    }

    public static <T> List<T> getListFromResp(ResponseEntity<ResultInfo<T>> responseEntity) {
        checkResp(responseEntity);
        return Optional.ofNullable(responseEntity.getBody()).orElse(ResultInfo.success()).getContent();
    }

    /**
     * 从Restful调用的结果中，获取Map
     *
     * @param responseEntity Restful返回的结果
     * @return java.util.Map 目标Mqp
     * @date 2022/2/27 11:43 PM
     */
    public static Map getMapFromResp(ResponseEntity<?> responseEntity) {
        checkResp(responseEntity);
        return responseEntity.getBody() instanceof ResultInfo<?> resultInfo ? resultInfo.getResultMap() : Collections.emptyMap();
    }

    public static Map getMapFromRespGRIC(ResponseEntity<ResultInfo<Map<String, Object>>> responseEntity) {
        checkResp(responseEntity);
        return Optional.ofNullable(responseEntity.getBody()).orElse(ResultInfo.success(Collections.emptyMap())).getResultMap();
    }

    /**
     * 针对通用的Restful返回，当响应不是200时，抛出运行时异常
     *
     * @param responseEntity /
     * @date 2022/2/28 1:44 PM
     */
    private static void checkResp(@NotNull ResponseEntity<?> responseEntity) {
        if (responseEntity.getBody() instanceof ResultInfo<?> resultInfo && !Objects.equals(resultInfo.getBusinessCode(), ResultCode.SUCCESS.code()))
            throw new BadRequestException(resultInfo.getDescription());
    }
}
