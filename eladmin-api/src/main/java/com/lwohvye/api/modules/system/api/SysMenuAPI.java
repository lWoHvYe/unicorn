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

import com.lwohvye.api.modules.system.domain.vo.MenuVo;
import com.lwohvye.api.modules.system.service.dto.MenuDto;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.api.modules.system.domain.Menu;
import com.lwohvye.api.modules.system.service.dto.MenuQueryCriteria;
import com.lwohvye.utils.result.ResultInfo;
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
public interface SysMenuAPI {

    @GetMapping("/api/sys/menus/build")
    ResponseEntity<List<MenuVo>> buildMenus();

    @GetMapping("/api/sys/menus/lazy")
    ResponseEntity<List<MenuDto>> query(@RequestParam Long pid);

    @GetMapping("/api/sys/menus/child")
    ResponseEntity<Set<Long>> child(@RequestParam Long id);

    @GetMapping("/api/sys/menus")
    ResponseEntity<ResultInfo<Map<String, Object>>> query(MenuQueryCriteria criteria) throws Exception;

    @PostMapping("/api/sys/menus/superior")
    ResponseEntity<List<MenuDto>> getSuperior(@RequestBody List<Long> ids);

    @PostMapping("/api/sys/menus")
    ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Menu resources);

    @PutMapping("/api/sys/menus")
    ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Menu resources);

    @DeleteMapping("/api/sys/menus")
    ResponseEntity<ResultInfo<String>> delete(@RequestBody Set<Long> ids);

}
