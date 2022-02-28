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

import com.lwohvye.annotation.log.Log;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.modules.mnt.domain.Database;
import com.lwohvye.modules.mnt.service.IDatabaseService;
import com.lwohvye.modules.mnt.service.dto.DatabaseDto;
import com.lwohvye.modules.mnt.service.dto.DatabaseQueryCriteria;
import com.lwohvye.modules.mnt.util.SqlUtils;
import com.lwohvye.utils.FileUtil;
import com.lwohvye.utils.result.ResultInfo;
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
import java.util.Set;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@Tag(name = "DatabaseController", description = "运维：数据库管理")
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/database")
public class DatabaseController {

    private final String fileSavePath = FileUtil.getTmpDirPath() + "/";
    private final IDatabaseService databaseService;

    @Operation(summary = "导出数据库数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, DatabaseQueryCriteria criteria) throws IOException {
        databaseService.download(databaseService.queryAll(criteria), response);
    }

    @Operation(summary = "查询数据库")
    @GetMapping
    public ResponseEntity<Object> query(DatabaseQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(databaseService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @Log("新增数据库")
    @Operation(summary = "新增数据库")
    @PostMapping
    public ResponseEntity<Object> create(@Validated @RequestBody Database resources) {
        databaseService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改数据库")
    @Operation(summary = "修改数据库")
    @PutMapping
    public ResponseEntity<Object> update(@Validated @RequestBody Database resources) {
        databaseService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除数据库")
    @Operation(summary = "删除数据库")
    @DeleteMapping
    public ResponseEntity<Object> delete(@RequestBody Set<String> ids) {
        databaseService.delete(ids);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @Log("测试数据库链接")
    @Operation(summary = "测试数据库链接")
    @PostMapping("/testConnect")
    public ResponseEntity<Object> testConnect(@Validated @RequestBody Database resources) {
        return new ResponseEntity<>(databaseService.testConnection(resources), HttpStatus.CREATED);
    }

    @Log("执行SQL脚本")
    @Operation(summary = "执行SQL脚本")
    @PostMapping(value = "/upload")
    public ResponseEntity<Object> upload(@RequestBody MultipartFile file, HttpServletRequest request) throws Exception {
        String id = request.getParameter("id");
        DatabaseDto database = databaseService.findById(id);
        String fileName;
        if (database != null) {
            fileName = file.getOriginalFilename();
            File executeFile = new File(fileSavePath + fileName);
            FileUtil.del(executeFile);
            file.transferTo(executeFile);
            String result = SqlUtils.executeFile(database.getJdbcUrl(), database.getUserName(), database.getPwd(), executeFile);
            return new ResponseEntity<>(result, HttpStatus.OK);
        } else {
            throw new BadRequestException("Database not exist");
        }
    }
}
