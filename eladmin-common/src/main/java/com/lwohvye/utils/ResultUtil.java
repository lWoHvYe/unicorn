package com.lwohvye.utils;

import lombok.SneakyThrows;
import org.springframework.http.MediaType;

import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;

/**
 * @description 主动返回数据
 * @date 2021/11/15 7:19 下午
 */
public class ResultUtil {


    @SneakyThrows
    public static void resultJson(HttpServletResponse response, int responseStatus, String msg) {
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setStatus(responseStatus);
        try (PrintWriter out = response.getWriter()) {
            out.append(msg);
        }
    }

}
