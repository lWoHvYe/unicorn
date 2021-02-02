package com.lwohvye.utils.result;

import cn.hutool.core.util.ObjectUtil;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import org.springframework.data.domain.Page;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

    public ResultInfo(long businessCode, T t, String description) {
        this.businessCode = businessCode;
        this.result = t;
        this.description = description;
        currentTime = LocalDateTime.now();
    }

    public ResultInfo(long businessCode, List<T> resultSet, String description) {
        this.businessCode = businessCode;
        this.content = resultSet;
        this.description = description;
        this.totalElements = resultSet.size();
        currentTime = LocalDateTime.now();
    }

    public ResultInfo(long businessCode, Map<String, Object> objectMap, String description) {
        this.businessCode = businessCode;
//        考虑map可以不是page的情况
        var content = objectMap.get("content");
        if (ObjectUtil.isNotNull(content) && content instanceof List)
            this.content = (List<T>) content;
        else
            this.resultMap = objectMap;
        var totalElements = objectMap.get("totalElements");
        if (ObjectUtil.isNotNull(totalElements) && totalElements instanceof Long)
            this.totalElements = (Long) totalElements;
        this.description = description;
        currentTime = LocalDateTime.now();
    }

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
     * 成功返回单一实体结果集。还是采用List的方式
     *
     * @param t
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> success(T t) {
//        如果是Map，走分页
        if (t instanceof Map)
            return new ResultInfo<>(ResultCode.SUCCESS.getCode(), (Map<String, Object>) t, "");
        if (t instanceof List)
            return new ResultInfo<>(ResultCode.SUCCESS.getCode(), (List<T>) t, "");
        if (t instanceof Page)
            return new ResultInfo<>(ResultCode.SUCCESS.getCode(), (Page<T>) t, "");

        return new ResultInfo<>(ResultCode.SUCCESS.getCode(), t, "");
    }

    /**
     * 成功返回分页结果集
     *
     * @param page
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> successPage(Map<String, Object> page) {
        return new ResultInfo<>(ResultCode.SUCCESS.getCode(), page, "");
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
