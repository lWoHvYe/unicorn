/*
 *  Copyright 2020-2022 lWoHvYe
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
