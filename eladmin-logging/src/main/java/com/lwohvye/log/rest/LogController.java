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
package com.lwohvye.log.rest;

import com.lwohvye.annotation.log.Log;
import com.lwohvye.log.service.ILogService;
import com.lwohvye.log.service.dto.LogQueryCriteria;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Zheng Jie
 * @date 2018-11-24
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/logs")
@Tag(name = "LogController", description = "系统：日志管理")
public class LogController {

    private final ILogService logService;

    @Operation(summary = "导出数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, LogQueryCriteria criteria) throws IOException {
        criteria.setLogType("INFO");
        logService.download(logService.queryAll(criteria), response);
    }

    @Operation(summary = "导出错误数据")
    @GetMapping(value = "/error/download")
    public void downloadErrorLog(HttpServletResponse response, LogQueryCriteria criteria) throws IOException {
        criteria.setLogType("ERROR");
        logService.download(logService.queryAll(criteria), response);
    }

    @GetMapping
    @Operation(summary = "日志查询")
    public ResponseEntity<Object> query(LogQueryCriteria criteria, Pageable pageable) {
        criteria.setLogType("INFO");
        return new ResponseEntity<>(ResultInfo.success(logService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @GetMapping(value = "/user")
    @Operation(summary = "用户日志查询")
    public ResponseEntity<Object> queryUserLog(LogQueryCriteria criteria, Pageable pageable) {
        criteria.setLogType("INFO");
        criteria.setBlurry(SecurityUtils.getCurrentUsername());
        return new ResponseEntity<>(ResultInfo.success(logService.queryAllByUser(criteria, pageable)), HttpStatus.OK);
    }

    @GetMapping(value = "/error")
    @Operation(summary = "错误日志查询")
    public ResponseEntity<Object> queryErrorLog(LogQueryCriteria criteria, Pageable pageable) {
        criteria.setLogType("ERROR");
        return new ResponseEntity<>(ResultInfo.success(logService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @GetMapping(value = "/error/{id}")
    @Operation(summary = "日志异常详情查询")
    public ResponseEntity<Object> queryErrorLogs(@PathVariable Long id) {
        return new ResponseEntity<>(ResultInfo.success(logService.findByErrDetail(id)), HttpStatus.OK);
    }

    @DeleteMapping(value = "/del/error")
    @Log("删除所有ERROR日志")
    @Operation(summary = "删除所有ERROR日志")
    public ResponseEntity<Object> delAllErrorLog() {
        logService.delAllByError();
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @DeleteMapping(value = "/del/info")
    @Log("删除所有INFO日志")
    @Operation(summary = "删除所有INFO日志")
    public ResponseEntity<Object> delAllInfoLog() {
        logService.delAllByInfo();
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
