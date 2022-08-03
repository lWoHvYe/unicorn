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

import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.api.modules.system.service.dto.RoleQueryCriteria;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Set;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
public interface SysRoleAPI {

    @GetMapping("/api/sys/roles/{id}")
    ResponseEntity<Object> query(@PathVariable Long id);

    @GetMapping("/api/sys/roles/all")
    ResponseEntity<Object> query();

    @GetMapping("/api/sys/roles")
    ResponseEntity<Object> query(RoleQueryCriteria criteria, Pageable pageable);

    @GetMapping("/api/sys/roles/level")
    ResponseEntity<Object> getLevel();

    @PostMapping("/api/sys/roles")
    ResponseEntity<Object> create(@Validated @RequestBody Role resources);

    @PutMapping("/api/sys/roles")
    ResponseEntity<Object> update(@Validated(Update.class) @RequestBody Role resources);

    @PutMapping("/api/sys/roles/menu")
    ResponseEntity<Object> updateMenu(@RequestBody Role resources);

    @DeleteMapping("/api/sys/roles")
    ResponseEntity<Object> delete(@RequestBody Set<Long> ids);

    @GetMapping("/api/sys/roles/uid/{userId}")
    ResponseEntity<Object> queryByUid(@PathVariable Long userId);
}
