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
package com.lwohvye.utils.result;

import cn.hutool.core.util.ObjectUtil;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.lwohvye.exception.BadRequestException;
import lombok.Getter;
import org.springframework.data.domain.Page;

import java.time.LocalDateTime;
import java.util.*;

/**
 * 通用返回对象
 * Created by cy on 2021/01/08.
 */
@Getter
public class ResultInfo<T> implements IResultInfo<T> {
    private static final long serialVersionUID = -7313465544799377989L;
    private long businessCode;
    private List<T> content;
    private T result;
    private Map resultMap;
    private String description;
    private long totalElements;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime currentTime;

    /**
     * @return
     * @description 普通实体，用result字段返回
     * @params [businessCode, t, description]
     * @author Hongyan Wang
     * @date 2021/2/6 8:33
     */
    public ResultInfo(long businessCode, T t, String description) {
        this.businessCode = businessCode;
        this.result = t;
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    /**
     * @return
     * @description list类依旧参照分页。包含content和totalElements
     * @params [businessCode, resultSet, description]
     * @author Hongyan Wang
     * @date 2021/2/6 8:36
     */
    public ResultInfo(long businessCode, List<T> resultSet, String description) {
        this.businessCode = businessCode;
        this.content = resultSet;
        this.description = description;
        this.totalElements = resultSet.size();
        currentTime = LocalDateTime.now();
    }

    /**
     * @return
     * @description map根据是否是page，如果是page类。返回content和totalElements；如果是普通map，返回resultMap
     * @params [businessCode, objectMap, description]
     * @author Hongyan Wang
     * @date 2021/2/6 8:37
     */
    public ResultInfo(long businessCode, Map<String, Object> objectMap, String description) {
        this.businessCode = businessCode;
//        考虑map可以不是page的情况
        var content = objectMap.get("content");
//        content可能是List。也可能是Set
        if (ObjectUtil.isNotNull(content)) {
            if (content instanceof List list)
                this.content = list;
            else if (content instanceof Set set)
//                Set类型转成List
                this.content = new ArrayList(set);
            else
                throw new BadRequestException("content类型有误，请求兼容新类型" + content.getClass());
        } else {
//            不是page用resultMap字段返回
            this.resultMap = objectMap;
        }
        var totalElements = objectMap.get("totalElements");
        if (ObjectUtil.isNotNull(totalElements) && totalElements instanceof Long total)
            this.totalElements = total;
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    /**
     * @return
     * @description page类返回content和totalElements
     * @params [businessCode, page, description]
     * @author Hongyan Wang
     * @date 2021/2/6 8:38
     */
    public ResultInfo(long businessCode, Page<T> page, String description) {
        this.businessCode = businessCode;
        this.content = page.getContent();
        this.totalElements = page.getTotalElements();
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    /**
     * @return com.lwohvye.utils.result.ResultInfo<T>
     * @description 只是返回成功状态。无其他结果集
     * @params []
     * @author Hongyan Wang
     * @date 2021/1/9 9:39
     */
    public static <T> ResultInfo<T> success() {
        return new ResultInfo<>(ResultCode.SUCCESS.getCode(), new ArrayList<>(), "");
    }

    /**
     * 成功返回不分页结果集
     *
     * @param resultSet
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> success(List<T> resultSet) {
        return new ResultInfo<>(ResultCode.SUCCESS.getCode(), resultSet, "");
    }

    /**
     * 通用成功返回方式。支持map、list、page和单一实体，但都会套个壳，针对不需要壳的场景不适用
     *
     * @param t
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> success(T t) {
//        如果是Map，走分页
        if (t instanceof Map map)
            return new ResultInfo<>(ResultCode.SUCCESS.getCode(), map, "");
//        List类，一般是非分页查询
        if (t instanceof List list)
            return new ResultInfo<>(ResultCode.SUCCESS.getCode(), list, "");
//        分页可能不是转成map的
        if (t instanceof Page page)
            return new ResultInfo<>(ResultCode.SUCCESS.getCode(), page, "");

        return new ResultInfo<>(ResultCode.SUCCESS.getCode(), t, "");
    }

    /**
     * 成功返回带描述的不分页结果集
     *
     * @param resultSet
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> success(List<T> resultSet, String description) {
        return new ResultInfo<>(ResultCode.SUCCESS.getCode(), resultSet, description);
    }

    /**
     * 带描述的服务端处理失败返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> failed(String description) {
        return new ResultInfo<>(ResultCode.FAILED.getCode(), new ArrayList<>(), description);
    }

    /**
     * 未登录或token过期的失败返回
     *
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> unauthorized() {
        return new ResultInfo<>(ResultCode.UNAUTHORIZED.getCode(), new ArrayList<>(), ResultCode.UNAUTHORIZED.getDescription());
    }

    /**
     * 未授权的失败返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> forbidden(String description) {
        return new ResultInfo<>(ResultCode.FORBIDDEN.getCode(), new ArrayList<>(), description);
    }

    /**
     * 参数验证失败的返回
     *
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> validateFailed() {
        return new ResultInfo<>(ResultCode.VALIDATE_FAILED.getCode(), new ArrayList<>(), ResultCode.VALIDATE_FAILED.getDescription());
    }

    /**
     * 带描述的参数验证失败返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> validateFailed(String description) {
        return new ResultInfo<>(ResultCode.VALIDATE_FAILED.getCode(), new ArrayList<>(), description);
    }

    /**
     * 请求方法错误的返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> methodNotAllowed(String description) {
        return new ResultInfo<>(ResultCode.METHOD_NOT_ALLOWED.getCode(), new ArrayList<>(), description);
    }
}
