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

import com.lwohvye.api.modules.mnt.domain.Deploy;
import com.lwohvye.api.modules.mnt.domain.DeployHistory;
import com.lwohvye.api.modules.mnt.service.dto.DeployQueryCriteria;
import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.core.utils.FileUtils;
import com.lwohvye.core.utils.result.ResultInfo;
import com.lwohvye.sys.modules.mnt.service.IDeployService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@Tag(name = "DeployController", description = "运维：部署管理")
@RestController
@RequestMapping("/api/deploy")
@RequiredArgsConstructor
public class DeployController {

    private final String fileSavePath = FileUtils.getTmpDirPath() + File.separator;
    private final IDeployService deployService;


    @Operation(summary = "导出部署数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, DeployQueryCriteria criteria) throws IOException {
        deployService.download(deployService.queryAll(criteria), response);
    }

    @RespResultBody
    @Operation(summary = "查询部署")
    @GetMapping
    public Map<String, Object> query(DeployQueryCriteria criteria, Pageable pageable) {
        return deployService.queryAll(criteria, pageable);
    }

    @OprLog("新增部署")
    @RespResultBody
    @Operation(summary = "新增部署")
    @PostMapping
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Deploy resources) {
        deployService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改部署")
    @Operation(summary = "修改部署")
    @PutMapping
    public ResponseEntity<ResultInfo<String>> update(@Validated @RequestBody Deploy resources) {
        deployService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除部署")
    @Operation(summary = "删除部署")
    @DeleteMapping
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        deployService.delete(ids);
        return ResultInfo.success();
    }

    @OprLog("上传文件部署")
    @Operation(summary = "上传文件部署")
    @PostMapping(value = "/upload")
    public ResponseEntity<Map<String, Object>> upload(@RequestBody MultipartFile file, HttpServletRequest request) throws Exception {
        Long id = Long.valueOf(request.getParameter("id"));
        String fileName = "";
        if (file != null) {
            fileName = file.getOriginalFilename();
            File deployFile = new File(fileSavePath + fileName);
            FileUtils.del(deployFile);
            file.transferTo(deployFile);
            //文件下一步要根据文件名字来
            deployService.deploy(fileSavePath + fileName, id);
        } else {
            System.out.println("没有找到相对应的文件");
        }
        System.out.println("文件上传的原名称为:" + Objects.requireNonNull(file).getOriginalFilename());
        return ResponseEntity.ok(Map.of("errno", 0, "id", fileName));
    }

    @OprLog("系统还原")
    @Operation(summary = "系统还原")
    @PostMapping(value = "/serverReduction")
    public ResponseEntity<String> serverReduction(@Validated @RequestBody DeployHistory resources) {
        String result = deployService.serverReduction(resources);
        return ResponseEntity.ok(result);
    }

    @OprLog("服务运行状态")
    @Operation(summary = "服务运行状态")
    @PostMapping(value = "/serverStatus")
    public ResponseEntity<String> serverStatus(@Validated @RequestBody Deploy resources) {
        String result = deployService.serverStatus(resources);
        return ResponseEntity.ok(result);
    }

    @OprLog("启动服务")
    @Operation(summary = "启动服务")
    @PostMapping(value = "/startServer")
    public ResponseEntity<String> startServer(@Validated @RequestBody Deploy resources) {
        String result = deployService.startServer(resources);
        return ResponseEntity.ok(result);
    }

    @OprLog("停止服务")
    @Operation(summary = "停止服务")
    @PostMapping(value = "/stopServer")
    public ResponseEntity<String> stopServer(@Validated @RequestBody Deploy resources) {
        String result = deployService.stopServer(resources);
        return ResponseEntity.ok(result);
    }
}
