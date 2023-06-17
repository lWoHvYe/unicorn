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
package com.lwohvye.generator.rest

import com.lwohvye.core.annotation.RespResultBody
import com.lwohvye.core.utils.result.ResultInfo
import com.lwohvye.generator.domain.GenConfig
import com.lwohvye.generator.service.IGenConfigService
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.tags.Tag
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.validation.annotation.Validated
import org.springframework.web.bind.annotation.*

/**
 * @author Zheng Jie
 * @date 2019-01-14
 */
@RestController
@RequestMapping("/api/genConfig")
@Tag(name = "GenConfigController", description = "系统：代码生成器配置管理")
class GenConfigController(val genConfigService: IGenConfigService) {
    @Operation(summary = "查询")
    @RespResultBody
    @GetMapping(value = ["/{tableName}"])
    fun query(@PathVariable tableName: String?): GenConfig? {
        return genConfigService.find(tableName)
    }

    @Operation(summary = "修改")
    @PutMapping
    fun update(@Validated @RequestBody genConfig: GenConfig): ResponseEntity<ResultInfo<String>> {
        genConfigService.update(genConfig.tableName, genConfig)
        return ResponseEntity(HttpStatus.NO_CONTENT)
    }
}
