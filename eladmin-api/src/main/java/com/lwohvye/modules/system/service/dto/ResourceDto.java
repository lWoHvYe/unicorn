/*
 *  Copyright 2019-2022 lWoHvYe
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
package com.lwohvye.modules.system.service.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @description /
 * @date 2021-11-27
 **/
@Data
public class ResourceDto implements Serializable {

    /** ID */
    /**
     * 防止精度丢失
     */
    @JsonSerialize(using = ToStringSerializer.class)
    private Long resourceId;

    /**
     * 资源名称
     */
    private String name;

    /**
     * URI
     */
    private String pattern;

    // 请求方法。空表示全部
    private String reqMethod;

    /**
     * 状态 0-不可用 1-可用
     */
    private Integer status;

    private List<String> roleCodes;

    /**
     * 所在类名
     */
    private String restName;

    /**
     * 备注
     */
    private String remark;
}
