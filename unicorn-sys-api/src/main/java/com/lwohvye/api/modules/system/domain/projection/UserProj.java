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

package com.lwohvye.api.modules.system.domain.projection;

import org.springframework.beans.factory.annotation.Value;

import java.util.Set;

public interface UserProj {

    Long getId();

    Set<RoleProj> getRoles();

    Set<BaseProj> getJobs();

    BaseProj getDept();

    String getUsername();

    String getNickName();

    String getEmail();

    String getPhone();

    String getGender();

    Boolean getEnabled();

    String getDescription();

    @Value("#{target.username + ' ' + target.phone}")
    String getUserInfo();

    default String getUserDetail() {
        return getUsername().concat(" ").concat(getPhone());
    }
}
