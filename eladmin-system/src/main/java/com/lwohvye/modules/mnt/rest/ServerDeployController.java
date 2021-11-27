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
package com.lwohvye.modules.mnt.rest;

import com.lwohvye.annotation.Log;
import com.lwohvye.modules.mnt.domain.ServerDeploy;
import com.lwohvye.modules.mnt.service.IServerDeployService;
import com.lwohvye.modules.mnt.service.dto.ServerDeployQueryCriteria;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Set;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@RestController
@Tag(name = "ServerDeployController", description = "运维：服务器管理")
@RequiredArgsConstructor
@RequestMapping("/api/serverDeploy")
public class ServerDeployController {

    private final IServerDeployService serverDeployService;

    @Operation(summary = "导出服务器数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, ServerDeployQueryCriteria criteria) throws IOException {
        serverDeployService.download(serverDeployService.queryAll(criteria), response);
    }

    @Operation(summary = "查询服务器")
    @GetMapping
    public ResponseEntity<Object> query(ServerDeployQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(serverDeployService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @Log("新增服务器")
    @Operation(summary = "新增服务器")
    @PostMapping
    public ResponseEntity<Object> create(@Validated @RequestBody ServerDeploy resources) {
        serverDeployService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改服务器")
    @Operation(summary = "修改服务器")
    @PutMapping
    public ResponseEntity<Object> update(@Validated @RequestBody ServerDeploy resources) {
        serverDeployService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除服务器")
    @Operation(summary = "删除Server")
    @DeleteMapping
    public ResponseEntity<Object> delete(@RequestBody Set<Long> ids) {
        serverDeployService.delete(ids);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @Log("测试连接服务器")
    @Operation(summary = "测试连接服务器")
    @PostMapping("/testConnect")
    public ResponseEntity<Object> testConnect(@Validated @RequestBody ServerDeploy resources) {
        return new ResponseEntity<>(ResultInfo.success(serverDeployService.testConnect(resources)), HttpStatus.CREATED);
    }
}
