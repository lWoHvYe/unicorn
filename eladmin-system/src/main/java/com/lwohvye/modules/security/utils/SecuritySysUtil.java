package com.lwohvye.modules.security.utils;

import com.lwohvye.config.LocalCoreConfig;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import com.lwohvye.modules.security.service.UserCacheClean;
import org.springframework.util.Assert;

/**
 * @author Hongyan Wang
 * @description system模块Security部分工具类/属性定义
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

    /**
     * 用户信息缓存
     *
     * @see {@link UserCacheClean}
     */
    //  这种缓存的方式，也许解决了些问题，但导致无法做集群的扩展，故调整为分布式缓存redis
//    static Map<String, JwtUserDto> userDtoCache = new ConcurrentHashMap<>();
    public static String getUserCacheKey() {
        // 用户缓存的redis key。考虑多系统，这里也要加标识
        return LocalCoreConfig.SYS_NAME + "Sys-User-JwtInfo-Cache";
    }
}
