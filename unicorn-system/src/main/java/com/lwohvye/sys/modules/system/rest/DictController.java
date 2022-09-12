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
package com.lwohvye.sys.modules.system.rest;

import com.lwohvye.api.modules.system.domain.Dict;
import com.lwohvye.api.modules.system.service.dto.DictDto;
import com.lwohvye.api.modules.system.service.dto.DictQueryCriteria;
import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.result.ResultInfo;
import com.lwohvye.sys.modules.system.service.IDictService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2019-04-10
 */
@Tag(name = "DictController", description = "系统：字典管理")
@RestController
@RequestMapping("/api/sys/dict")
@RespResultBody
@RequiredArgsConstructor
public class DictController {

    private final IDictService dictService;
    private static final String ENTITY_NAME = "dict";

    @Operation(summary = "查询字典")
    @GetMapping(value = "/all")
    public List<DictDto> queryAll() {
        return dictService.queryAll(new DictQueryCriteria());
    }

    @Operation(summary = "查询字典")
    @GetMapping
    public Map<String, Object> query(DictQueryCriteria resources, Pageable pageable) {
        return dictService.queryAll(resources, pageable);
    }

    @OprLog("新增字典")
    @Operation(summary = "新增字典")
    @PostMapping
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Dict resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        dictService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改字典")
    @Operation(summary = "修改字典")
    @PutMapping
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Dict resources) {
        dictService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除字典")
    @Operation(summary = "删除字典")
    @DeleteMapping
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        dictService.delete(ids);
        return ResultInfo.success();
    }
}
