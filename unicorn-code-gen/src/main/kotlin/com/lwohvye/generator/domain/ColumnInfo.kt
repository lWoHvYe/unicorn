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
package com.lwohvye.generator.domain

import com.lwohvye.generator.utils.GenUtil
import io.swagger.v3.oas.annotations.media.Schema
import jakarta.persistence.*
import java.io.Serializable

/**
 * 列的数据信息
 * @author Zheng Jie
 * @date 2019-01-02
 */
@Entity
@Table(name = "code_column_config")
class ColumnInfo(
    @field:Schema(description = "表名") val tableName: String?,
    @field:Schema(description = "数据库字段名称") val columnName: String,
    @field:Schema(description = "是否必填") var notNull: Boolean,
    @field:Schema(description = "数据库字段类型") var columnType: String,
    remark: String?,
    @field:Schema(description = "数据库字段键类型") var keyType: String?,
    @field:Schema(description = "字段额外的参数") var extra: String?
) : Serializable {
    @Id
    @Column(name = "column_id")
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long? = null

    @Schema(description = "数据库字段描述")
    var remark: String?

    @Schema(description = "是否在列表显示")
    var listShow: Boolean

    @Schema(description = "是否表单显示")
    var formShow: Boolean

    @Schema(description = "表单类型")
    var formType: String? = null

    @Schema(description = "查询 1:模糊 2：精确")
    var queryType: String? = null

    @Schema(description = "字典名称")
    var dictName: String? = null

    @Schema(description = "日期注解")
    var dateAnnotation: String? = null

    init {
        if (GenUtil.PK.equals(keyType, ignoreCase = true) && GenUtil.EXTRA.equals(extra, ignoreCase = true)) {
            notNull = false
        }
        this.remark = remark
        listShow = true
        formShow = true
    }
}
