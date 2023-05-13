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
import com.lwohvye.sys.modules.mnt.service.dto.DeployHistoryQueryCriteria;
import com.lwohvye.sys.modules.mnt.service.IDeployHistoryService;
import com.lwohvye.core.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@Tag(name = "DeployHistoryController", description = "运维：部署历史管理")
@RestController
@RequestMapping("/api/deployHistory")
@RequiredArgsConstructor
public class DeployHistoryController {

    private final IDeployHistoryService deployHistoryService;

    @Operation(summary = "导出部署历史数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, DeployHistoryQueryCriteria criteria) throws IOException {
        deployHistoryService.download(deployHistoryService.queryAll(criteria), response);
    }

    @Operation(summary = "查询部署历史")
    @RespResultBody
    @GetMapping
    public Map<String, Object> query(DeployHistoryQueryCriteria criteria, Pageable pageable) {
        return deployHistoryService.queryAll(criteria, pageable);
    }

    @OprLog("删除DeployHistory")
    @Operation(summary = "删除部署历史")
    @DeleteMapping
    public ResultInfo<String> delete(@RequestBody Set<String> ids) {
        deployHistoryService.delete(ids);
        return ResultInfo.success();
    }
}
