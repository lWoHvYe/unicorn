/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.lwohvye.tools.domain.vo

import com.fasterxml.jackson.annotation.JsonIgnore
import org.springframework.web.multipart.MultipartFile
import java.util.*

data class MailVo(
    val id: String?,

    // 发送方
    var from: String?,

    // 接收方
    var to: String?,

    // 主题
    var subject: String?,

    // 正文
    var text: String?,


    ) {

    var cc: String? = null
    var bcc: String? = null

    // 发送时间
    var sentDate: Date? = null

    // 发送状态
    var status: String? = null

    // 错误信息
    var error: String? = null

    // 附件
    @JsonIgnore
    var multipartFiles: Array<MultipartFile>? = null
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is MailVo) return false

        if (id != other.id) return false
        if (from != other.from) return false
        if (to != other.to) return false
        if (subject != other.subject) return false
        return cc == other.cc
    }

    override fun hashCode(): Int {
        var result = id?.hashCode() ?: 0
        result = 31 * result + (from?.hashCode() ?: 0)
        result = 31 * result + (to?.hashCode() ?: 0)
        result = 31 * result + (subject?.hashCode() ?: 0)
        result = 31 * result + (cc?.hashCode() ?: 0)
        return result
    }

    override fun toString(): String {
        return "MailVo(id=$id, from=$from, to=$to, subject=$subject, sentDate=$sentDate, cc=$cc, status=$status)"
    }


}
