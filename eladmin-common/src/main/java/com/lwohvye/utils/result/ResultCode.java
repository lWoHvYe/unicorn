package com.lwohvye.utils.result;

/**
 * 枚举了一些常用API返回码
 * Created by cy on 2021/01/08.
 */
public enum ResultCode implements IResultCode {
    SUCCESS(200, "操作成功"),
    FAILED(500, "操作失败"),
    VALIDATE_FAILED(400, "参数检验失败"),
    UNAUTHORIZED(401, "未登录或token已经过期"),
    FORBIDDEN(403, "无权限"),
    METHOD_NOT_ALLOWED(405, "方法不允许");
    private long code;
    private String description;

    ResultCode(long code, String description) {
        this.code = code;
        this.description = description;
    }

    public long getCode() {
        return code;
    }

    public String getDescription() {
        return description;
    }
}
