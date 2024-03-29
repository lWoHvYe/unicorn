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
package com.lwohvye.sys.modules.system.rest;

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.api.modules.system.api.SysResourceAPI;
import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.api.modules.system.service.dto.ResourceDto;
import com.lwohvye.api.modules.system.service.dto.ResourceQueryCriteria;
import com.lwohvye.sys.modules.system.service.IResourceService;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
@Tag(name = "ResourceController", description = "资源管理")
@RestController
@RespResultBody
@RequiredArgsConstructor
public class ResourceController implements SysResourceAPI {

    private final IResourceService resourceService;

    @Operation(summary = "查询资源")
    @Override
    public Map<String, Object> query(ResourceQueryCriteria criteria, Pageable pageable) {
        return resourceService.queryAll(criteria, pageable);
    }

    @OprLog("新增资源")
    @Operation(summary = "新增资源")
    @Override
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Resource resources) {
        resourceService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改资源")
    @Operation(summary = "修改资源")
    @Override
    public ResponseEntity<ResultInfo<String>> update(@Validated @RequestBody Resource resources) {
        resourceService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除资源")
    @Operation(summary = "删除资源")
    @Override
    public ResultInfo<String> delete(@RequestBody Long[] ids) {
        resourceService.deleteAll(ids);
        return ResultInfo.success();
    }

    public List<ResourceDto> queryAllRes() {
        return resourceService.queryAllRes();
    }
}
