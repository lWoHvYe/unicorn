package com.lwohvye.config;

import cn.hutool.core.util.StrUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class LocalCoreConfig {

    //    系统名称。影响redis的key的前缀
    public static String SYS_NAME;

    @Value("${local.sys.name:}")
    public void setSysName(String sysName) {
        SYS_NAME = StrUtil.isNotEmpty(sysName) ? "[-" + sysName + "-]::" : "";
    }

    public static String getSysName() {
        return SYS_NAME;
    }
}
