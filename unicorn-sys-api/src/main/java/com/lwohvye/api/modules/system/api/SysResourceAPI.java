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

import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.api.modules.system.service.dto.ResourceDto;
import com.lwohvye.api.modules.system.service.dto.ResourceQueryCriteria;
import com.lwohvye.core.utils.result.ResultInfo;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
public interface SysResourceAPI {


    @GetMapping("/api/sys/resources")
    ResponseEntity<ResultInfo<Map<String, Object>>> query(ResourceQueryCriteria criteria, Pageable pageable);

    @PostMapping("/api/sys/resources")
    ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Resource resources);

    @PutMapping("/api/sys/resources")
    ResponseEntity<ResultInfo<String>> update(@Validated @RequestBody Resource resources);

    @DeleteMapping("/api/sys/resources")
    ResponseEntity<ResultInfo<String>> delete(@RequestBody Long[] ids);

    @GetMapping("/api/sys/resources/queryAllRes")
    ResponseEntity<ResultInfo<ResourceDto>> queryAllRes();
}
