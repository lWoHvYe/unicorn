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
import com.lwohvye.api.modules.system.domain.Job;
import com.lwohvye.api.modules.system.service.dto.JobQueryCriteria;
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
public interface SysJobAPI {

    @GetMapping("/api/sys/job")
    ResponseEntity<Object> query(JobQueryCriteria criteria, Pageable pageable);

    @PostMapping("/api/sys/job")
    ResponseEntity<Object> create(@Validated @RequestBody Job resources);

    @PutMapping("/api/sys/job")
    ResponseEntity<Object> update(@Validated(Update.class) @RequestBody Job resources);

    @DeleteMapping("/api/sys/job")
    ResponseEntity<Object> delete(@RequestBody Set<Long> ids);

}
