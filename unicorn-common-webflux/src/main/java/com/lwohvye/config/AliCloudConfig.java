/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
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
