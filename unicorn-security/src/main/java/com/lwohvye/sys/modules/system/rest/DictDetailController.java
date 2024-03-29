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

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.api.modules.system.domain.DictDetail;
import com.lwohvye.sys.modules.system.service.IDictDetailService;
import com.lwohvye.api.modules.system.service.dto.DictDetailDto;
import com.lwohvye.api.modules.system.service.dto.DictDetailQueryCriteria;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Zheng Jie
 * @date 2019-04-10
 */
@Tag(name = "DictDetailController", description = "系统：字典详情管理")
@RestController
@RequestMapping("/api/sys/dictDetail")
@RespResultBody
@RequiredArgsConstructor
public class DictDetailController {

    private final IDictDetailService dictDetailService;
    private static final String ENTITY_NAME = "dictDetail";

    @Operation(summary = "查询字典详情")
    @GetMapping
    public Map<String, Object> query(DictDetailQueryCriteria criteria, @PageableDefault(sort = {"dictSort"}, direction = Sort.Direction.ASC) Pageable pageable) {
        return dictDetailService.queryAll(criteria, pageable);
    }

    @Operation(summary = "查询多个字典详情")
    @GetMapping(value = "/map")
    public Map<String, List<DictDetailDto>> getDictDetailMaps(@RequestParam String dictName) {
        String[] names = dictName.split("[,，]");
        Map<String, List<DictDetailDto>> dictMap = new HashMap<>(16);
        for (String name : names) {
            dictMap.put(name, dictDetailService.getDictByName(name));
        }
        return dictMap;
    }

    @OprLog("新增字典详情")
    @Operation(summary = "新增字典详情")
    @PostMapping
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody DictDetail resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        dictDetailService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改字典详情")
    @Operation(summary = "修改字典详情")
    @PutMapping
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody DictDetail resources) {
        dictDetailService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除字典详情")
    @Operation(summary = "删除字典详情")
    @DeleteMapping(value = "/{id}")
    public ResultInfo<String> delete(@PathVariable Long id) {
        dictDetailService.delete(id);
        return ResultInfo.success();
    }
}
