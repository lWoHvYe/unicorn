/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.beans.config;

import cn.hutool.core.util.StrUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class LocalPropertyConfig {

    //    系统名称。影响redis的key的前缀
    public static String SYS_NAME;

    @Value("${local.sys.name:}")
    public void setSysName(String sysName) {
        SYS_NAME = StrUtil.isNotEmpty(sysName) ? "-n_n-" + sysName + "-n_n-::" : "";
    }

    public static String getSysName() {
        return SYS_NAME;
    }

    // 这里的情景就是当集群部署时，针对单个事件，多个相同的实例只要消费一次就可以了。但这里因为要通知各实例更新内部的缓存，需要每个实例都消费。当下想到的就是每个实例一个队列，通过配置
    public static String SP_SYNC_DELAY_QUEUE;

    public static String ORIGIN; // 实例标识

    @Value("${local.sys.sp-sync-queue:}") // Value无法直接为静态属性注入值，需放在set方法上
    public void setSpSyncVal(String spSyncQueue) {
        SP_SYNC_DELAY_QUEUE = spSyncQueue;
        ORIGIN = StringUtils.hasText(spSyncQueue) ? spSyncQueue.split("\\.")[0] : "";
    }
}
