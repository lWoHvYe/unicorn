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
package com.lwohvye.modules.kafka.entity;

import cn.hutool.core.util.IdUtil;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.concurrent.TimeUnit;

@Data
@Accessors(chain = true)
public class DelayMessage {
    /**
     * 事件唯一ID,用于去重检查
     */
    private String eventId = IdUtil.fastSimpleUUID();

    /**
     * 有效期
     */
    private Long time;

    /**
     * 单位
     */
    private TimeUnit unit;

    /**
     * 内容
     */
    private String context;

    /**
     * 真实Topic
     */
    private String actualTopic;
}
