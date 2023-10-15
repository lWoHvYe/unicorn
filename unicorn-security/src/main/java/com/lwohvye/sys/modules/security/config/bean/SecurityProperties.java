/*
 *  Copyright 2019-2020 Zheng Jie
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
package com.lwohvye.sys.modules.security.config.bean;

import lombok.Data;

/**
 * Jwt参数配置
 *
 * @author Zheng Jie
 * @date 2019年11月28日
 */
@Data
public class SecurityProperties {

    /**
     * Request Headers ： Authorization
     */
    private String header = "Authorization";

    /**
     * 令牌前缀，最后留个空格 Bearer
     */
    private String tokenStartWith = "Bearer";

    /**
     * 令牌过期时间 此处单位/秒。默认2小时
     */
    private Long tokenValidityInSeconds = 7200L;

    /**
     * 验证码 key
     */
    private String codeKey = "code-key-";

    /**
     * token 失效前通知检查，单位/秒。默认15分钟
     */
    private Long detect = 900L;

    // 登录即将过期通知，单系统只通知一次
    private String expireNoticeKey = "expire-notice-token-";

    /**
     * 前缀后面要加个空格，这个无法在文件中配置，所以重写下get方法
     */
    public String getTokenStartWith() {
        return tokenStartWith + " ";
    }
}
