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
     * @date 2021/6/12 3:31 下午
     */
    public static String getAuthToken(SecurityProperties properties, String token) {
        Assert.notNull(properties, "系统错误。请联系研发处理");
        Assert.notNull(token, "token不可为空");
        return properties.getOnlineKey() + LocalCoreConfig.SYS_NAME + token;
    }

    /**
     * @param properties
     * @return java.lang.String
     * @description 存储已⏰了过期的token
     * @date 2021/11/13 10:42 上午
     */
    public static String getExpireNoticeKey(SecurityProperties properties) {
        Assert.notNull(properties, "系统错误。请联系研发处理");
        return properties.getExpireNoticeKey() + LocalCoreConfig.SYS_NAME;
    }

}
