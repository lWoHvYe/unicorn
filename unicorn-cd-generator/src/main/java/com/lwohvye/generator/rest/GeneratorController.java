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
package com.lwohvye.generator.rest;

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.generator.domain.ColumnInfo;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.generator.service.IGenConfigService;
import com.lwohvye.generator.service.IGeneratorService;
import com.lwohvye.core.utils.PageUtils;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author Zheng Jie
 * @date 2019-01-02
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/generator")
@Tag(name = "GeneratorController", description = "系统：代码生成管理")
public class GeneratorController {

    private final IGeneratorService generatorService;
    private final IGenConfigService genConfigService;

    @Value("${generator.enabled:true}")
    private Boolean generatorEnabled;

    @Operation(summary = "查询数据库数据")
    @GetMapping(value = "/tables/all")
    public ResponseEntity<List> queryTables() {
        return new ResponseEntity<>(generatorService.getTables(), HttpStatus.OK);
    }

    @Operation(summary = "查询数据库数据")
    @RespResultBody
    @GetMapping(value = "/tables")
    public Map<String, Object> queryTables(@RequestParam(defaultValue = "") String name,
                                           @RequestParam(defaultValue = "0") Integer page,
                                           @RequestParam(defaultValue = "10") Integer size) {
        int[] startEnd = PageUtils.transToStartEnd(page, size);
        return generatorService.getTables(name, startEnd);
    }

    @Operation(summary = "查询字段数据")
    @RespResultBody
    @GetMapping(value = "/columns")
    public Map<String, Object> queryColumns(@RequestParam String tableName) {
        List<ColumnInfo> columnInfos = generatorService.getColumns(tableName);
        return PageUtils.toPage(columnInfos, columnInfos.size());
    }

    @Operation(summary = "保存字段数据")
    @RespResultBody
    @PutMapping
    public ResponseEntity<ResultInfo<String>> save(@RequestBody List<ColumnInfo> columnInfos) {
        generatorService.save(columnInfos);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Operation(summary = "同步字段数据")
    @PostMapping(value = "sync")
    public ResponseEntity<ResultInfo<String>> sync(@RequestBody List<String> tables) {
        for (String table : tables) {
            generatorService.sync(generatorService.getColumns(table), generatorService.query(table));
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Operation(summary = "生成代码")
    @RespResultBody
    @PostMapping(value = "/{tableName}/{type}")
    public List<Map<String, Object>> generator(@PathVariable String tableName, @PathVariable Integer type, HttpServletRequest request, HttpServletResponse response) {
        if (Boolean.FALSE.equals(generatorEnabled) && type == 0) {
            throw new BadRequestException("此环境不允许生成代码，请选择预览或者下载查看！");
        }
        switch (type) {
            // 生成代码
            case 0:
                generatorService.generator(genConfigService.find(tableName), generatorService.getColumns(tableName));
                break;
            // 预览
            case 1:
                return generatorService.preview(genConfigService.find(tableName), generatorService.getColumns(tableName));
            // 打包
            case 2:
                generatorService.download(genConfigService.find(tableName), generatorService.getColumns(tableName), request, response);
                break;
            default:
                throw new BadRequestException("没有这个选项");
        }
        return Collections.emptyList();
    }
}
