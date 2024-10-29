/*
 *  Copyright 2019-2022 lWoHvYe
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
package com.lwohvye.api.modules.system.api;

import cn.hutool.core.lang.Dict;
import com.lwohvye.api.modules.system.service.dto.RoleDto;
import com.lwohvye.api.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.api.modules.system.service.dto.RoleQueryCriteria;
import com.lwohvye.core.utils.result.ResultInfo;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.service.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
@HttpExchange(url = "/api/sys/roles")
public interface SysRoleAPI {

    @GetExchange("/{id}")
    RoleDto query(@PathVariable Long id);

    @GetExchange("/all")
    List<RoleDto> query();

    @GetExchange
    Map<String, Object> query(RoleQueryCriteria criteria, Pageable pageable);

    @GetExchange("/level")
    Dict getLevel();

    @PostExchange
    ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Role resources);

    @PutExchange
    ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Role resources);

    @PutExchange("/menu")
    ResponseEntity<ResultInfo<String>> updateMenu(@RequestBody Role resources);

    @DeleteExchange
    ResultInfo<String> delete(@RequestBody Set<Long> ids);

    @GetExchange("/uid/{userId}")
    List<RoleSmallDto> queryByUid(@PathVariable Long userId);
}
