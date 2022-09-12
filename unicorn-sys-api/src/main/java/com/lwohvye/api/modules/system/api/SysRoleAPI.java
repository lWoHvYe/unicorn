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

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
public interface SysRoleAPI {

    @GetMapping("/api/sys/roles/{id}")
    RoleDto query(@PathVariable Long id);

    @GetMapping("/api/sys/roles/all")
    List<RoleDto> query();

    @GetMapping("/api/sys/roles")
    Map<String, Object> query(RoleQueryCriteria criteria, Pageable pageable);

    @GetMapping("/api/sys/roles/level")
    Dict getLevel();

    @PostMapping("/api/sys/roles")
    ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Role resources);

    @PutMapping("/api/sys/roles")
    ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Role resources);

    @PutMapping("/api/sys/roles/menu")
    ResponseEntity<ResultInfo<String>> updateMenu(@RequestBody Role resources);

    @DeleteMapping("/api/sys/roles")
    ResultInfo<String> delete(@RequestBody Set<Long> ids);

    @GetMapping("/api/sys/roles/uid/{userId}")
    List<RoleSmallDto> queryByUid(@PathVariable Long userId);
}
