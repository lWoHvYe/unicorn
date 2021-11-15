package com.lwohvye.utils;

import lombok.SneakyThrows;

import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;

/**
 * @description 主动返回数据
 * @date 2021/11/15 7:19 下午
 */
public class ResultUtil {


    @SneakyThrows
    public static void resultJson(HttpServletResponse response, int responseStatus, String msg) {
        response.setCharacterEncoding("UTF-8");
        response.setContentType("application/json; charset=utf-8");
        response.setStatus(responseStatus);
        try (PrintWriter out = response.getWriter()) {
            out.append(msg);
        }
    }

}
