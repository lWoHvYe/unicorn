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

import cn.hutool.core.util.ObjectUtil;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.lwohvye.core.exception.BadRequestException;
import lombok.Getter;
import lombok.ToString;
import org.springframework.data.domain.Page;

import java.io.Serial;
import java.time.LocalDateTime;
import java.util.*;

/**
 * 通用返回对象。与统一异常处理。配套的统一异常处理{@link com.lwohvye.beans.advice.ResponseResultBodyAdvice}类
 * Created by cy on 2021/01/08.
 */
@Getter
@ToString
@SuppressWarnings({"unchecked", "rawtypes", "unused"})
public class ResultInfo<T> implements IResultInfo<T> {
    @Serial
    private static final long serialVersionUID = -2022L;
    private final int businessCode;
    private List<T> content;
    private T result;
    private Map resultMap;
    private final String description;
    private long totalElements;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private final LocalDateTime currentTime;

    /**
     * 普通实体，用result字段返回
     *
     * @params [businessCode, t, description]
     * @date 2021/2/6 8:33
     */
    public ResultInfo(int businessCode, T t, String description) {
        this.businessCode = businessCode;
        this.result = t;
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    /**
     * list类依旧参照分页。包含content和totalElements
     *
     * @params [businessCode, resultSet, description]
     * @date 2021/2/6 8:36
     */
    public ResultInfo(int businessCode, List<T> resultSet, String description) {
        this.businessCode = businessCode;
        this.content = resultSet;
        this.description = description;
        this.totalElements = resultSet.size();
        currentTime = LocalDateTime.now();
    }

    /**
     * map根据是否是page，如果是page类。返回content和totalElements；如果是普通map，返回resultMap
     *
     * @params [businessCode, objectMap, description]
     * @date 2021/2/6 8:37
     */
    public ResultInfo(int businessCode, Map<String, Object> objectMap, String description) {
        this.businessCode = businessCode;
//        考虑map可以不是page的情况
        var ctt = objectMap.get("content"); // 内容主体
//        content可能是List。也可能是Set
        if (ObjectUtil.isNotNull(ctt)) {
            if (ctt instanceof List list)
                this.content = list;
            else if (ctt instanceof Set set)
//                Set类型转成List
                this.content = new ArrayList(set);
            else
                throw new BadRequestException("content类型有误，请求兼容新类型" + ctt.getClass());
        } else {
//            不是page用resultMap字段返回
            this.resultMap = objectMap;
        }
        var tes = objectMap.get("totalElements"); // 总记录数
        if (ObjectUtil.isNotNull(tes) && tes instanceof Long total)
            this.totalElements = total;
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    /**
     * page类返回content和totalElements
     *
     * @params [businessCode, page, description]
     * @date 2021/2/6 8:38
     */
    public ResultInfo(int businessCode, Page<T> page, String description) {
        this.businessCode = businessCode;
        this.content = page.getContent();
        this.totalElements = page.getTotalElements();
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    /**
     * 只是返回成功状态。无其他结果集
     *
     * @return com.lwohvye.utils.result.ResultInfo
     * @date 2021/1/9 9:39
     */
    public static <T> ResultInfo<T> success() {
        return new ResultInfo<>(ResultCode.SUCCESS.code(), new ArrayList<>(), "操作成功");
    }

    /**
     * 通用成功返回方式。支持map、list、page和单一实体，但都会套个壳，针对不需要壳的场景不适用
     *
     * @param t   /
     * @param <T> /
     * @return /
     */
    public static <T> ResultInfo<T> success(T t) {
        return ResultInfo.success(t, "");
    }

    public static <T> ResultInfo<T> success(T t, String description) {
//        如果是Map，走分页
        if (t instanceof Map map)
            return new ResultInfo<>(ResultCode.SUCCESS.code(), map, description);
//        List类，一般是非分页查询
        if (t instanceof List list)
            return new ResultInfo<>(ResultCode.SUCCESS.code(), list, description);
//        分页可能不是转成map的
        if (t instanceof Page page)
            return new ResultInfo<>(ResultCode.SUCCESS.code(), page, description);

        return new ResultInfo<>(ResultCode.SUCCESS.code(), t, description);
    }

    /**
     * 成功返回不分页结果集
     *
     * @param resultSet /
     * @param <T>       /
     * @return /
     */
    public static <T> ResultInfo<T> success(List<T> resultSet) {
        return new ResultInfo<>(ResultCode.SUCCESS.code(), resultSet, "");
    }

    /**
     * 成功返回带描述的不分页结果集
     *
     * @param resultSet   /
     * @param description /
     * @param <T>         /
     * @return /
     */
    public static <T> ResultInfo<T> success(List<T> resultSet, String description) {
        return new ResultInfo<>(ResultCode.SUCCESS.code(), resultSet, description);
    }

    /**
     * 带描述的服务端处理失败返回
     *
     * @param description /
     * @param <T>         /
     * @return /
     */
    public static <T> ResultInfo<T> failed(String description) {
        return new ResultInfo<>(ResultCode.FAILED.code(), Collections.emptyList(), description);
    }

    /**
     * 未登录或token过期的失败返回
     *
     * @param <T> /
     * @return /
     */
    public static <T> ResultInfo<T> unauthorized() {
        return new ResultInfo<>(ResultCode.UNAUTHORIZED.code(), Collections.emptyList(), ResultCode.UNAUTHORIZED.description());
    }

    /**
     * 未授权的失败返回
     *
     * @param description /
     * @param <T>         /
     * @return /
     */
    public static <T> ResultInfo<T> forbidden(String description) {
        return new ResultInfo<>(ResultCode.FORBIDDEN.code(), Collections.emptyList(), description);
    }

    /**
     * 参数验证失败的返回
     *
     * @param <T> /
     * @return /
     */
    public static <T> ResultInfo<T> validateFailed() {
        return new ResultInfo<>(ResultCode.VALIDATE_FAILED.code(), Collections.emptyList(), ResultCode.VALIDATE_FAILED.description());
    }

    /**
     * 带描述的参数验证失败返回
     *
     * @param description /
     * @param <T>         /
     * @return /
     */
    public static <T> ResultInfo<T> validateFailed(String description) {
        return new ResultInfo<>(ResultCode.VALIDATE_FAILED.code(), Collections.emptyList(), description);
    }

    /**
     * 请求方法错误的返回
     *
     * @param description /
     * @param <T>         /
     * @return /
     */
    public static <T> ResultInfo<T> methodNotAllowed(String description) {
        return new ResultInfo<>(ResultCode.METHOD_NOT_ALLOWED.code(), Collections.emptyList(), description);
    }
}
