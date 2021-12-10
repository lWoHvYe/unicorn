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
package com.lwohvye.modules.system.rest;

import com.lwohvye.log.annotation.Log;
import com.lwohvye.modules.system.domain.Resource;
import com.lwohvye.modules.system.service.IResourceService;
import com.lwohvye.modules.system.service.dto.ResourceQueryCriteria;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
@RestController
@RequiredArgsConstructor
@Tag(name = "ResourceController", description = "资源管理")
@RequestMapping("/api/sys/resources")
public class ResourceController {

    private final IResourceService resourceService;

    @GetMapping
    @Log("查询资源")
    @Operation(summary = "查询资源")
    public ResponseEntity<Object> query(ResourceQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(resourceService.queryAll(criteria, pageable), HttpStatus.OK);
    }

    @PostMapping
    @Log("新增资源")
    @Operation(summary = "新增资源")
    public ResponseEntity<Object> create(@Validated @RequestBody Resource resources) {
        return new ResponseEntity<>(resourceService.create(resources), HttpStatus.CREATED);
    }

    @PutMapping
    @Log("修改资源")
    @Operation(summary = "修改资源")
    public ResponseEntity<Object> update(@Validated @RequestBody Resource resources) {
        resourceService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Log("删除资源")
    @Operation(summary = "删除资源")
    @DeleteMapping
    public ResponseEntity<Object> delete(@RequestBody Long[] ids) {
        resourceService.deleteAll(ids);
        return new ResponseEntity<>(HttpStatus.OK);
    }
}
