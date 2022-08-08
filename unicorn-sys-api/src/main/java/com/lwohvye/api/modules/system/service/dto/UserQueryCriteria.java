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
package com.lwohvye.api.modules.system.service.dto;

import cn.hutool.core.util.StrUtil;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.core.utils.StringUtils;
import lombok.Data;
import com.lwohvye.core.annotation.Query;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Data
public class UserQueryCriteria implements Serializable {

    @Query
    private Long id;

    @Query(propName = "id", type = Query.Type.IN, joinName = "dept")
    private Set<Long> deptIds = new HashSet<>();

    @Query(blurry = "email,username,nickName")
    private String blurry;

    @Query
    private Boolean enabled;

    private Long deptId;

    @Query(type = Query.Type.BETWEEN)
    private List<Timestamp> createTime;

    private String usernameStr;

    @Query(propName = "username", type = Query.Type.IN_INNER_LIKE)
    private List<String> usernames;

    //    前端通过role.field传多个属性。在同一join里通过多条件过滤。针对long类型值为-1的，做is null查询。
    @Query(propName = "role", type = Query.Type.EQUAL_IN_MULTI_JOIN, joinName = "roles")
    private Role role;

    // 库中使用Base64存储，做模糊查询（业务不建议。因为无法使用索引，效率很低，这里只是提供一种调用库函数的方式）
    @Query(type = Query.Type.FUNCTION_4_EQUAL, functionName = "from_base64")
    private String description;

    /**
     * 重写set方法。将前端传的逗号分割的username，转成字符集合，并设置到另一个字段中
     *
     * @param usernameStr
     * @date 2021/3/10 22:12
     */
    public void setUsernameStr(String usernameStr) {
        this.usernames = StrUtil.isNotEmpty(usernameStr) ? StringUtils.parseStrToArrString(usernameStr) : null;
    }
}
