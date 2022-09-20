/*
*  Copyright 2019-2022 lWoHvYe
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
package ${package}.rest;

import com.lwohvye.core.annotation.log.OprLog;
import ${package}.domain.${className};
import ${package}.service.${className}Service;
import ${package}.service.dto.${className}QueryCriteria;
import org.springframework.data.domain.Pageable;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.io.IOException;
import javax.servlet.http.HttpServletResponse;

/**
* @website https://lwohvye.com
* @author ${author}
* @date ${date}
**/
@RestController
@RequiredArgsConstructor
@Tag(name = "${className}Controller", description = "${apiAlias}管理")
@RequestMapping("/api/${changeClassName}")
public class ${className}Controller {

    private final I${className}Service ${changeClassName}Service;

    @Operation(summary = "导出数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, ${className}QueryCriteria criteria) throws IOException {
        ${changeClassName}Service.download(${changeClassName}Service.queryAll(criteria), response);
    }

    @GetMapping
    @Operation(summary = "查询${apiAlias}")
    public Map<String, Object> query(${className}QueryCriteria criteria, Pageable pageable){
        return ${changeClassName}Service.queryAll(criteria, pageable);
    }

    @PostMapping
    @Log("新增${apiAlias}")
    @Operation(summary = "新增${apiAlias}")
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody ${className} resources){
        ${changeClassName}Service.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @PutMapping
    @Log("修改${apiAlias}")
    @Operation(summary = "修改${apiAlias}")
    public ResponseEntity<ResultInfo<String>> update(@Validated @RequestBody ${className} resources){
        ${changeClassName}Service.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Log("删除${apiAlias}")
    @Operation(summary = "删除${apiAlias}")
    @DeleteMapping
    public ResultInfo<String> delete(@RequestBody ${pkColumnType}[] ids) {
        ${changeClassName}Service.deleteAll(ids);
        return ResultInfo.success();
    }
}
