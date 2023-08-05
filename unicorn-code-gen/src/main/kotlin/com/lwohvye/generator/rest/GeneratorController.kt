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
import com.lwohvye.core.exception.BadRequestException
import com.lwohvye.core.utils.PageUtils
import com.lwohvye.core.utils.result.ResultInfo
import com.lwohvye.generator.domain.ColumnInfo
import com.lwohvye.generator.service.IGenConfigService
import com.lwohvye.generator.service.IGeneratorService
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.tags.Tag
import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse
import org.springframework.beans.factory.annotation.Value
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

/**
 * @author Zheng Jie
 * @date 2019-01-02
 */
@RestController
@RequestMapping("/api/generator")
@Tag(name = "GeneratorController", description = "系统：代码生成管理")
class GeneratorController(val generatorService: IGeneratorService, val genConfigService: IGenConfigService) {

    @Value("\${generator.enabled:true}")
    private val generatorEnabled: Boolean = true

    @Operation(summary = "查询数据库数据")
    @GetMapping(value = ["/tables/all"])
    fun queryTables(): ResponseEntity<List<*>?> {
        return ResponseEntity(generatorService.tables, HttpStatus.OK)
    }

    @Operation(summary = "查询数据库数据")
    @RespResultBody
    @GetMapping(value = ["/tables"])
    fun queryTables(
        @RequestParam(defaultValue = "") name: String,
        @RequestParam(defaultValue = "0") page: Int?,
        @RequestParam(defaultValue = "10") size: Int?
    ): Map<String?, Any?>? {
        val startEnd = PageUtils.transToStartEnd(page!!, size!!)
        return generatorService.getTables(name, startEnd)
    }

    @Operation(summary = "查询字段数据")
    @RespResultBody
    @GetMapping(value = ["/columns"])
    fun queryColumns(@RequestParam tableName: String): Map<String, Any> {
        val columnInfos = generatorService.getColumns(tableName)
        return PageUtils.toPage(columnInfos, columnInfos!!.size)
    }

    @Operation(summary = "保存字段数据")
    @RespResultBody
    @PutMapping
    fun save(@RequestBody columnInfos: List<ColumnInfo?>): ResponseEntity<ResultInfo<String>> {
        generatorService.save(columnInfos)
        return ResponseEntity(HttpStatus.CREATED)
    }

    @Operation(summary = "同步字段数据")
    @PostMapping(value = ["sync"])
    fun sync(@RequestBody tables: List<String?>): ResponseEntity<ResultInfo<String>> {
        for (table in tables) {
            generatorService.sync(table?.let { generatorService.getColumns(it) },
                table?.let { generatorService.query(it) })
        }
        return ResponseEntity(HttpStatus.NO_CONTENT)
    }

    @Operation(summary = "生成代码")
    @RespResultBody
    @PostMapping(value = ["/{tableName}/{type}"])
    fun generator(
        @PathVariable tableName: String,
        @PathVariable type: Int,
        request: HttpServletRequest,
        response: HttpServletResponse
    ): List<Map<String, Any>> {
        if (java.lang.Boolean.FALSE == generatorEnabled && type == 0) {
            throw BadRequestException("此环境不允许生成代码，请选择预览或者下载查看！")
        }
        val tableNamePlain = tableName.replace(Regex("\\."), "").replace("/", "")
        when (type) {
            0 -> generatorService.generator(
                genConfigService.find(tableNamePlain),
                generatorService.getColumns(tableNamePlain)
            )

            1 -> return generatorService.preview(
                genConfigService.find(tableNamePlain),
                generatorService.getColumns(tableNamePlain)
            )

            2 -> generatorService.download(
                genConfigService.find(tableNamePlain),
                generatorService.getColumns(tableNamePlain),
                request,
                response
            )

            else -> throw BadRequestException("没有这个选项")
        }
        return emptyList()
    }
}
