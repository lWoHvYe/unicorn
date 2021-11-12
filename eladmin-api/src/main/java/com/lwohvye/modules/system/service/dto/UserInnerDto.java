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
import java.util.Date;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Getter
@Setter
@RequiredArgsConstructor
public class UserInnerDto implements Serializable {

    private Long id;

    private Set<RoleSmallDto> roles;

    private Set<JobSmallDto> jobs;

    private DeptSmallDto dept;

    private String username;

    private String nickName;

    private String email;

    private String phone;

    private String gender;

    private String avatarName;

    private String avatarPath;

    private String password;

    private Boolean enabled;

    private Boolean isAdmin = false;

    private String description;

    private Date pwdResetTime;

    public UserInnerDto(UserDto userDto) {
        // 设置其他属性
        Arrays.stream(ReflectUtil.getFields(UserInnerDto.class))
                // field时Dto的，因为Inner和她已经没有关系了，所以要用fieldName来设置属性
                .iterator().forEachRemaining(field -> ReflectUtil.setFieldValue(this, field, ReflectUtil.getFieldValue(userDto, field.getName())));
    }

}
