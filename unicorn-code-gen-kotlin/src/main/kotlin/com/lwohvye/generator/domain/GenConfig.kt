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

import io.swagger.v3.oas.annotations.media.Schema
import jakarta.persistence.*
import jakarta.validation.constraints.NotBlank
import lombok.experimental.Accessors
import java.io.Serializable

/**
 * 代码生成配置
 * @author Zheng Jie
 * @date 2019-01-03
 */
@Entity
@Accessors(chain = true)
@Table(name = "code_gen_config")
class GenConfig(@field:Schema(description = "表名") val tableName: @NotBlank String?) : Serializable {
    @Id
    @Column(name = "config_id")
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long? = null

    @Schema(description = "接口名称")
    var apiAlias: String? = null

    @Schema(description = "包路径")
    var pack: @NotBlank String? = null

    @Schema(description = "模块名")
    var moduleName: @NotBlank String? = null

    @Schema(description = "前端文件路径")
    var path: @NotBlank String? = null

    @Schema(description = "前端文件路径")
    var apiPath: String? = null

    @Schema(description = "作者")
    var author: String? = null

    @Schema(description = "表前缀")
    var prefix: String? = null

    @Schema(description = "是否覆盖")
    var cover = false
}
