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
package com.lwohvye.generator.service

import com.lwohvye.generator.domain.ColumnInfo
import com.lwohvye.generator.domain.GenConfig
import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse

/**
 * @author Zheng Jie
 * @date 2019-01-02
 */
interface IGeneratorService {
    /**
     * 查询数据库元数据
     *
     * @param name     表名
     * @param startEnd 分页参数
     * @return /
     */
    fun getTables(name: String, startEnd: IntArray): Map<String?, Any?>?

    /**
     * 得到数据表的元数据
     *
     * @param name 表名
     * @return /
     */
    fun getColumns(name: String): List<ColumnInfo?>?

    /**
     * 同步表数据
     *
     * @param columnInfos    /
     * @param columnInfoList /
     */
    fun sync(columnInfos: List<ColumnInfo?>?, columnInfoList: List<ColumnInfo?>?)

    /**
     * 保持数据
     *
     * @param columnInfos /
     */
    fun save(columnInfos: List<ColumnInfo?>)

    /**
     * 获取所有table
     *
     * @return /
     */
    val tables: List<*>?

    /**
     * 代码生成
     *
     * @param genConfig 配置信息
     * @param columns   字段信息
     */
    fun generator(genConfig: GenConfig?, columns: List<ColumnInfo?>?)

    /**
     * 预览
     *
     * @param genConfig 配置信息
     * @param columns   字段信息
     * @return /
     */
    fun preview(genConfig: GenConfig?, columns: List<ColumnInfo?>?): List<Map<String, Any>>

    /**
     * 打包下载
     *
     * @param genConfig 配置信息
     * @param columns   字段信息
     * @param request   /
     * @param response  /
     */
    fun download(
        genConfig: GenConfig?,
        columns: List<ColumnInfo?>?,
        request: HttpServletRequest?,
        response: HttpServletResponse?
    )

    /**
     * 查询数据库的表字段数据数据
     *
     * @param table /
     * @return /
     */
    fun query(table: String): List<ColumnInfo?>
}
