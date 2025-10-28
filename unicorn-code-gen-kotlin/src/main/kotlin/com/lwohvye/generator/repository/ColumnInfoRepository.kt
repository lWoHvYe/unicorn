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
package com.lwohvye.generator.repository

import com.lwohvye.generator.domain.ColumnInfo
import org.springframework.data.jpa.repository.JpaRepository

/**
 * @author Zheng Jie
 * @date 2019-01-14
 */
interface ColumnInfoRepository : JpaRepository<ColumnInfo, Long> {
    /**
     * 查询表信息
     * @param tableName 表格名
     * @return 表信息
     */
    fun findByTableNameOrderByIdAsc(tableName: String): List<ColumnInfo>?
}
