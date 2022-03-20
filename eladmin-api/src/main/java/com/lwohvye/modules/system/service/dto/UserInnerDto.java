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

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Getter
@Setter
@ToString
@RequiredArgsConstructor
public class UserInnerDto implements Serializable {

    private Long id;

    private List<Long> roleIds;

    private List<Long> jobIds;

    private Long deptId;

    private String username;

    // @JsonIgnore  // swarm化后，网关负责鉴权的化，这边就不能ignore了
    private String password;

    private Boolean enabled;

    // @JsonIgnore
    private Boolean isAdmin = false;

    private String description;

    private Date pwdResetTime;

}
