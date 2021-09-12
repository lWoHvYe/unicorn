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
package com.lwohvye.modules.system.service.dto;

import cn.hutool.core.util.ReflectUtil;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.util.Arrays;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Getter
@Setter
@RequiredArgsConstructor
public class UserInnerDto extends UserDto implements Serializable {

    private Long id;

    private String username;

    //    @JSONField(serialize = false)
    private String password;

    private Boolean enabled;

    //    @JSONField(serialize = false)
    private Boolean isAdmin = false;

    public UserInnerDto(UserDto userDto) {
        // 本类中这部分与父类重复的属性，在反射时，会被设置到父类.属性 上，所以需要单独设置
        this.id = userDto.getId();
        this.username = userDto.getUsername();
        this.password = userDto.getPassword();
        this.enabled = userDto.getEnabled();
        this.isAdmin = userDto.getIsAdmin();

        // 设置其他属性
        Arrays.stream(ReflectUtil.getFields(UserDto.class))
                .iterator().forEachRemaining(field -> ReflectUtil.setFieldValue(this, field, ReflectUtil.getFieldValue(userDto, field)));
    }

}
