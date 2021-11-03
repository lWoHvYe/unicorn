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
package com.lwohvye.modules.system.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @description 用户类型的枚举
 * @author Hongyan Wang
 * @date 2021年11月02日 16:51
 */
@Getter
@AllArgsConstructor
public enum UserTypeEnum {

    ADMIN(1, "尊贵的VIP"),
    NORMAL(0, "未来的VIP"),
    DEV(-1, "Ctrl C + V");

    private Integer type;

    private String desc;
}
