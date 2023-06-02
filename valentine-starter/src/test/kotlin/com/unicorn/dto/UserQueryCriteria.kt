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
package com.unicorn.dto

import cn.hutool.core.util.StrUtil
import com.lwohvye.core.utils.StringUtils
import com.unicorn.annotation.Query
import java.io.Serializable
import java.sql.Timestamp

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
class UserQueryCriteria : Serializable {
    @Query
    private val id: Long? = 1L

    @Query(propName = "id", type = Query.Type.IN, joinName = "dept")
    private val deptIds: Set<Long> = setOf(1L, 2L, 3L)

    @Query(blurry = "email,username,nickName")
    private val blurry: String? = "ABC"

    @Query
    private val enabled: Boolean? = null
    private val deptId: Long? = null

    @Query(type = Query.Type.BETWEEN)
    private val createTime: List<Timestamp>? = null
    private val usernameStr: String? = null

    @Query(propName = "username", type = Query.Type.IN_INNER_LIKE)
    private var usernames: List<String>? = null

    // region multiQuery in single joinTable Test
    @Query(propName = "code", type = Query.Type.INNER_LIKE, joinName = "roles")
    private val roleCode: String? = "ADMIN"

    @Query(propName = "level", type = Query.Type.GREATER_THAN, joinName = "roles")
    private val roleLevel: Long? = 2L

    @Query(propName = "enabled", type = Query.Type.GREATER_THAN, joinName = "roles>depts")
    private val roleDeptEnable: Boolean? = true

    // endregion
    // 库中使用Base64存储，做模糊查询（业务不建议。因为无法使用索引，效率很低，这里只是提供一种调用库函数的方式）
//    @Query(type = Query.Type.FUNCTION_4_EQUAL, functionName = "from_base64")
//    private val description: String? = "Hello"

    /**
     * 重写set方法。将前端传的逗号分割的username，转成字符集合，并设置到另一个字段中
     *
     * @param usernameStr
     * @date 2021/3/10 22:12
     */
    fun setUsernameStr(usernameStr: String?) {
        usernames = if (StrUtil.isNotEmpty(usernameStr)) StringUtils.parseStrToArrString(usernameStr) else null
    }
}
