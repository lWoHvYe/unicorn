package com.lwohvye.config;

import org.springframework.beans.factory.annotation.Value;

public class AliCloudConfig {
    public static String ACCESS_KEY_ID;

    public static String ACCESS_KEY_SECRET;

    @Value("${alibaba.cloud.access-key:}")
    public void setAccessKeyId(String accessKeyId) {
        ACCESS_KEY_ID = accessKeyId;
    }

    @Value("${alibaba.cloud.secret-key:}")
    public void setAccessKeySecret(String accessKeySecret) {
        ACCESS_KEY_SECRET = accessKeySecret;
    }
}
