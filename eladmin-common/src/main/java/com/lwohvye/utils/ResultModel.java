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

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;

/**
 * @packageName com.lwohvye.util
 * @className ResultBean
 * @description 通用的数据返回，及数据获取异常处理，优化返回逻辑。与{@link com.lwohvye.aop.ControllerAopAspect} 配合使用
 * 这个方式建议和日志的合起来。放到一个切面里处理。但无法处理返回是void的。
 * 可考虑使用MallService2B中的ResultInfo相关，配合统一的异常处理来替换
 * @date 2020/1/14 8:41
 */
@Getter
@Setter
@ToString
@Slf4j
public class ResultModel<T> implements Serializable {

    private static final long serialVersionUID = 1L;

    //    执行成功结果代码
    public static final int SUCCESS_CODE = 1001;
    //    执行失败结果代码
    public static final int FAILED_CODE = 1002;
    //    sql相关异常，
    public static final int SQL_ERROR_CODE = 1003;
    //    权限不足
    public static final int NEED_PERMISSION = 1004;
    //    通用自定义异常
    public static final int BAD_REQUEST = 1005;
    //    成功返回信息
    public static final String SUCCESS_MSG = "操作成功";
    //    失败返回信息
    public static final String FAILED_MSG = "操作失败，代码执行出错";
    //    sql相关错误信息
    public static final String SQL_ERROR_MSG = "Sql执行出错，语法错误";
    //    当前用户权限不足
    public static final String NEED_PERMISSION_MSG = "操作失败，当前用户权限不足";
    //    通用异常提示信息
    public static final String BAD_REQUEST_MSG = "操作失败，程序通用错误";
    //    返回数据
    private T data;
    //    结果信息
    private String msg = SUCCESS_MSG;
    //    结果代码
    private int code = SUCCESS_CODE;
    //    成功标识
    private boolean flag = true;

    public ResultModel() {
        super();
    }

    public ResultModel(T data) {
        super();
        this.data = data;
    }

    public ResultModel(Throwable e) {
        super();
        this.flag = false;
        this.code = FAILED_CODE;
        this.msg = FAILED_MSG;
//        输出错误信息到日志
        log.error(e.toString());
    }
}
