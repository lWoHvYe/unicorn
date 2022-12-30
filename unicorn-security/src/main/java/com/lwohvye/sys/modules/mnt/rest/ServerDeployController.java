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

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.api.modules.mnt.domain.ServerDeploy;
import com.lwohvye.sys.modules.mnt.service.IServerDeployService;
import com.lwohvye.api.modules.mnt.service.dto.ServerDeployQueryCriteria;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@Tag(name = "ServerDeployController", description = "运维：服务器管理")
@RestController
@RequestMapping("/api/serverDeploy")
@RequiredArgsConstructor
public class ServerDeployController {

    private final IServerDeployService serverDeployService;

    @Operation(summary = "导出服务器数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, ServerDeployQueryCriteria criteria) throws IOException {
        serverDeployService.download(serverDeployService.queryAll(criteria), response);
    }

    @Operation(summary = "查询服务器")
    @RespResultBody
    @GetMapping
    public Map<String, Object> query(ServerDeployQueryCriteria criteria, Pageable pageable) {
        return serverDeployService.queryAll(criteria, pageable);
    }

    @OprLog("新增服务器")
    @Operation(summary = "新增服务器")
    @RespResultBody
    @PostMapping
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody ServerDeploy resources) {
        serverDeployService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改服务器")
    @Operation(summary = "修改服务器")
    @PutMapping
    public ResponseEntity<ResultInfo<String>> update(@Validated @RequestBody ServerDeploy resources) {
        serverDeployService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除服务器")
    @Operation(summary = "删除Server")
    @DeleteMapping
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        serverDeployService.delete(ids);
        return ResultInfo.success();
    }

    @OprLog("测试连接服务器")
    @Operation(summary = "测试连接服务器")
    @PostMapping("/testConnect")
    public ResultInfo<Boolean> testConnect(@Validated @RequestBody ServerDeploy resources) {
        var res = serverDeployService.testConnect(resources);
        return ResultInfo.success(res);
    }
}
