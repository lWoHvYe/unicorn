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

import com.lwohvye.core.annotation.Query;
import lombok.Data;

import java.sql.Timestamp;
import java.util.List;

/**
* @author Zheng Jie
* @date 2019-03-25
*/
@Data
public class DeptQueryCriteria{

    @Query(type = Query.Type.INNER_LIKE)
    private String name;

    @Query
    private Boolean enabled;

    @Query
    private Long pid;

    // 默认true，非全部数据权限时，移除掉
    @Query(type = Query.Type.IS_NULL, propName = "pid")
    private Boolean pidIsNull = true;

    @Query(type = Query.Type.BETWEEN)
    private List<Timestamp> createTime;

    // 重写set，当其他属性有值是，不只查一级节点
    public void setName(String name) {
        this.name = name;
        this.pidIsNull = null;
    }

    public void setPid(Long pid) {
        this.pid = pid;
        this.pidIsNull = null;
    }

    public void setCreateTime(List<Timestamp> createTime) {
        this.createTime = createTime;
        this.pidIsNull = null;
    }
}
