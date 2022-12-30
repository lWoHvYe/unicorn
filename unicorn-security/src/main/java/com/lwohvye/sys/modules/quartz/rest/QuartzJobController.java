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
package com.lwohvye.sys.modules.quartz.rest;

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.api.modules.quartz.domain.QuartzJob;
import com.lwohvye.sys.modules.quartz.service.IQuartzJobService;
import com.lwohvye.api.modules.quartz.service.dto.JobQueryCriteria;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
 * @author Zheng Jie
 * @date 2019-01-07
 */
@Tag(name = "QuartzJobController", description = "系统：定时任务管理")
@Slf4j
@RestController
@RequestMapping("/api/jobs")
@RespResultBody
@RequiredArgsConstructor
public class QuartzJobController {

    private static final String ENTITY_NAME = "quartzJob";
    private final IQuartzJobService quartzJobService;

    @Operation(summary = "查询定时任务")
    @GetMapping
    public Map<String, Object> query(JobQueryCriteria criteria, Pageable pageable) {
        return quartzJobService.queryAll(criteria, pageable);
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
    public Map<String, Object> queryJobLog(JobQueryCriteria criteria, Pageable pageable) {
        return quartzJobService.queryAllLog(criteria, pageable);
    }

    @OprLog("新增定时任务")
    @Operation(summary = "新增定时任务")
    @PostMapping
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody QuartzJob resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        quartzJobService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改定时任务")
    @Operation(summary = "修改定时任务")
    @PutMapping
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody QuartzJob resources) {
        quartzJobService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("更改定时任务状态")
    @Operation(summary = "更改定时任务状态")
    @PutMapping(value = "/{id}")
    public ResponseEntity<ResultInfo<String>> update(@PathVariable Long id) {
        quartzJobService.updateIsPause(quartzJobService.findById(id));
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("执行定时任务")
    @Operation(summary = "执行定时任务")
    @PutMapping(value = "/exec/{id}")
    public ResponseEntity<ResultInfo<String>> execution(@PathVariable Long id) {
        quartzJobService.execution(quartzJobService.findById(id));
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除定时任务")
    @Operation(summary = "删除定时任务")
    @DeleteMapping
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        quartzJobService.delete(ids);
        return ResultInfo.success();
    }
}
