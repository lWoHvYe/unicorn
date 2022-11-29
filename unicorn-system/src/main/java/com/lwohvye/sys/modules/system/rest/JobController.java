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
import com.lwohvye.api.modules.system.api.SysJobAPI;
import com.lwohvye.api.modules.system.domain.Job;
import com.lwohvye.sys.modules.system.service.IJobService;
import com.lwohvye.api.modules.system.service.dto.JobQueryCriteria;
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
 * @author Zheng Jie
 * @date 2019-03-29
 */
@Tag(name = "JobController", description = "系统：岗位管理")
@RestController
@RespResultBody
@RequiredArgsConstructor
public class JobController implements SysJobAPI {

    private final IJobService jobService;
    private static final String ENTITY_NAME = "job";

    @Operation(summary = "导出岗位数据")
    @GetMapping(value = "/api/sys/job/download")
    public void download(HttpServletResponse response, JobQueryCriteria criteria) throws IOException {
        jobService.download(jobService.queryAll(criteria), response);
    }

    @Operation(summary = "查询岗位")
    @Override
    public Map<String, Object> query(JobQueryCriteria criteria, Pageable pageable) {
        return jobService.queryAll(criteria, pageable);
    }

    @OprLog("新增岗位")
    @Operation(summary = "新增岗位")
    @Override
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Job resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        jobService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改岗位")
    @Operation(summary = "修改岗位")
    @Override
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Job resources) {
        jobService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除岗位")
    @Operation(summary = "删除岗位")
    @Override
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        // 验证是否被用户关联
        jobService.verification(ids);
        jobService.delete(ids);
        return ResultInfo.success();
    }
}
