package com.lwohvye.utils.result;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;

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
    private List<T> resultSet;
    private String description;
    private long count;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime currentTime;

    public ResultInfo(long businessCode, List<T> resultSet, String description) {
        this.businessCode = businessCode;
        this.resultSet = resultSet;
        this.description = description;
        this.count = resultSet.size();
        currentTime = LocalDateTime.now();
    }

    public ResultInfo(long businessCode, Map<String, Object> page, String description) {
        this.businessCode = businessCode;
        this.resultSet = (List<T>) page.get("content");
        this.count = (Long) page.get("totalElements");
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
        return new ResultInfo<T>(ResultCode.SUCCESS.getCode(), new ArrayList<>(), "");
    }

    /**
     * 成功返回不分页结果集
     *
     * @param resultSet
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> success(List<T> resultSet) {
        return new ResultInfo<T>(ResultCode.SUCCESS.getCode(), resultSet, "");
    }

    /**
     * 成功返回单一实体结果集。还是采用List的方式
     *
     * @param result
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> success(T result) {
        List<T> list = new ArrayList<>();
        list.add(result);
        return new ResultInfo<T>(ResultCode.SUCCESS.getCode(), list, "");
    }

    /**
     * 成功返回分页结果集
     *
     * @param page
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> successPage(Map<String, Object> page) {
        return new ResultInfo<T>(ResultCode.SUCCESS.getCode(), page, "");
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
        return new ResultInfo<T>(ResultCode.SUCCESS.getCode(), resultSet, description);
    }

    /**
     * 带描述的服务端处理失败返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> failed(String description) {
        return new ResultInfo<T>(ResultCode.FAILED.getCode(), new ArrayList<>(), description);
    }

    /**
     * 未登录或token过期的失败返回
     *
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> unauthorized() {
        return new ResultInfo<T>(ResultCode.UNAUTHORIZED.getCode(), new ArrayList<>(), ResultCode.UNAUTHORIZED.getDescription());
    }

    /**
     * 未授权的失败返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> forbidden(String description) {
        return new ResultInfo<T>(ResultCode.FORBIDDEN.getCode(), new ArrayList<>(), description);
    }

    /**
     * 参数验证失败的返回
     *
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> validateFailed() {
        return new ResultInfo<T>(ResultCode.VALIDATE_FAILED.getCode(), new ArrayList<>(), ResultCode.VALIDATE_FAILED.getDescription());
    }

    /**
     * 带描述的参数验证失败返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> validateFailed(String description) {
        return new ResultInfo<T>(ResultCode.VALIDATE_FAILED.getCode(), new ArrayList<>(), description);
    }

    /**
     * 请求方法错误的返回
     *
     * @param description
     * @param <T>
     * @return
     */
    public static <T> ResultInfo<T> methodNotAllowed(String description) {
        return new ResultInfo<T>(ResultCode.METHOD_NOT_ALLOWED.getCode(), new ArrayList<>(), description);
    }
}
