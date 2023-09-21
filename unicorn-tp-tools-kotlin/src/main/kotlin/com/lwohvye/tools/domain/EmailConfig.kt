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
package com.lwohvye.tools.domain

import io.swagger.v3.oas.annotations.media.Schema
import jakarta.persistence.Column
import jakarta.persistence.Entity
import jakarta.persistence.Id
import jakarta.persistence.Table
import jakarta.validation.constraints.NotBlank
import java.io.Serializable

/**
 * 邮件配置类，数据存覆盖式存入数据存
 * @author Zheng Jie
 * @date 2018-12-26
 */
@Entity
@Table(name = "tool_email_config")
class EmailConfig : Serializable {
    @Id
    @Column(name = "config_id")
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    var id: Long? = null

    @Schema(description = "邮件服务器SMTP地址")
    var host: @NotBlank String? = null

    @Schema(description = "邮件服务器 SMTP 端口")
    var port: @NotBlank String? = null

    @Schema(description = "发件者用户名")
    var user: @NotBlank String? = null

    @Schema(description = "密码")
    var pass: @NotBlank String? = null

    @Schema(description = "收件人")
    var fromUser: @NotBlank String? = null
}
