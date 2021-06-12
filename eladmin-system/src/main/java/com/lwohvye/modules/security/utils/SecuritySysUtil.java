package com.lwohvye.modules.security.utils;

import com.lwohvye.config.LocalCoreConfig;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import org.springframework.util.Assert;

/**
 * @author Hongyan Wang
 * @description system模块Security部分工具类
 * @date 2021/6/12 3:36 下午
 */
public class SecuritySysUtil {

    /**
     * @param properties
     * @param token
     * @return java.lang.String
     * @description AuthToken格式
     * @author Hongyan Wang
     * @date 2021/6/12 3:31 下午
     */
    public static String getAuthToken(SecurityProperties properties, String token) {
        Assert.notNull(properties, "系统错误。请联系研发处理");
        Assert.notNull(token, "token不可为空");
        return properties.getOnlineKey() + LocalCoreConfig.SYS_NAME + token;
    }
}
