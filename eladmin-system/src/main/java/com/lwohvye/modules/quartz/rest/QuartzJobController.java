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
package com.lwohvye.modules.quartz.rest;

import com.lwohvye.annotation.Log;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.modules.quartz.domain.QuartzJob;
import com.lwohvye.modules.quartz.service.IQuartzJobService;
import com.lwohvye.modules.quartz.service.dto.JobQueryCriteria;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
 * @author Zheng Jie
 * @date 2019-01-07
 */
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/jobs")
@Tag(name = "QuartzJobController", description = "系统：定时任务管理")
public class QuartzJobController {

    private static final String ENTITY_NAME = "quartzJob";
    private final IQuartzJobService quartzJobService;

    @Operation(summary = "查询定时任务")
    @GetMapping
    public ResponseEntity<Object> query(JobQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(quartzJobService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @Operation(summary = "导出任务数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, JobQueryCriteria criteria) throws IOException {
        quartzJobService.download(quartzJobService.queryAll(criteria), response);
    }

    @Operation(summary = "导出日志数据")
    @GetMapping(value = "/logs/download")
    public void downloadLog(HttpServletResponse response, JobQueryCriteria criteria) throws IOException {
        quartzJobService.downloadLog(quartzJobService.queryAllLog(criteria), response);
    }

    @Operation(summary = "查询任务执行日志")
    @GetMapping(value = "/logs")
    public ResponseEntity<Object> queryJobLog(JobQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(quartzJobService.queryAllLog(criteria, pageable)), HttpStatus.OK);
    }

    @Log("新增定时任务")
    @Operation(summary = "新增定时任务")
    @PostMapping
    public ResponseEntity<Object> create(@Validated @RequestBody QuartzJob resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        quartzJobService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改定时任务")
    @Operation(summary = "修改定时任务")
    @PutMapping
    public ResponseEntity<Object> update(@Validated(Update.class) @RequestBody QuartzJob resources) {
        quartzJobService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("更改定时任务状态")
    @Operation(summary = "更改定时任务状态")
    @PutMapping(value = "/{id}")
    public ResponseEntity<Object> update(@PathVariable Long id) {
        quartzJobService.updateIsPause(quartzJobService.findById(id));
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("执行定时任务")
    @Operation(summary = "执行定时任务")
    @PutMapping(value = "/exec/{id}")
    public ResponseEntity<Object> execution(@PathVariable Long id) {
        quartzJobService.execution(quartzJobService.findById(id));
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除定时任务")
    @Operation(summary = "删除定时任务")
    @DeleteMapping
    public ResponseEntity<Object> delete(@RequestBody Set<Long> ids) {
        quartzJobService.delete(ids);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
