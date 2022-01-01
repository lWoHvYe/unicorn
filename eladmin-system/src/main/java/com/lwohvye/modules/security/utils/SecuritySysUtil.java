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
package com.lwohvye.modules.security.utils;

import com.lwohvye.config.LocalCoreConfig;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import org.springframework.util.Assert;

/**
 * @author Hongyan Wang
 * @description system模块Security部分工具类/属性定义
 * @date 2021/6/12 3:36 下午
 */
public class SecuritySysUtil {

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
