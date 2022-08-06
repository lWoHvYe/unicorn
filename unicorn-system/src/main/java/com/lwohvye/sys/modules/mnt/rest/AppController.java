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
package com.lwohvye.sys.modules.mnt.rest;

import com.lwohvye.annotation.log.Log;
import com.lwohvye.api.modules.mnt.domain.App;
import com.lwohvye.api.modules.mnt.service.dto.AppQueryCriteria;
import com.lwohvye.sys.modules.mnt.service.IAppService;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@RestController
@RequiredArgsConstructor
@Tag(name = "AppController", description = "运维：应用管理")
@RequestMapping("/api/app")
public class AppController {

    private final IAppService appService;

    @Operation(summary = "导出应用数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, AppQueryCriteria criteria) throws IOException {
        appService.download(appService.queryAll(criteria), response);
    }

    @Operation(summary = "查询应用")
    @GetMapping
    public ResponseEntity<ResultInfo<Map<String, Object>>> query(AppQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(appService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @Log("新增应用")
    @Operation(summary = "新增应用")
    @PostMapping
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody App resources) {
        appService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改应用")
    @Operation(summary = "修改应用")
    @PutMapping
    public ResponseEntity<ResultInfo<String>> update(@Validated @RequestBody App resources) {
        appService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除应用")
    @Operation(summary = "删除应用")
    @DeleteMapping
    public ResponseEntity<ResultInfo<String>> delete(@RequestBody Set<Long> ids) {
        appService.delete(ids);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
