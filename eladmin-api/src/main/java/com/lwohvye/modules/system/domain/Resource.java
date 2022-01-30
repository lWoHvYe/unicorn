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
package com.lwohvye.modules.system.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.lwohvye.base.BaseEntity;
import lombok.Data;
import cn.hutool.core.bean.BeanUtil;
import io.swagger.v3.oas.annotations.media.Schema;
import cn.hutool.core.bean.copier.CopyOptions;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.persistence.*;
import javax.validation.constraints.*;
import java.io.Serializable;
import java.util.Set;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
@Entity
@Getter
@Setter
@ToString
@Accessors(chain = true)
@Table(name = "sys_resource")
public class Resource implements Serializable {

    @Id
    @Column(name = "resource_id")
    @NotNull(groups = {BaseEntity.Update.class})
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long resourceId;

    @JsonIgnore
    @ManyToMany(mappedBy = "resources")
    @Schema(description = "资源角色")
    private Set<Role> roles;

    @Column(name = "name")
    @Schema(description = "资源名称")
    private String name;

    @Column(name = "pattern", nullable = false)
    @NotBlank
    @Schema(description = "URI")
    private String pattern;

    @Column(name = "req_method")
    @Schema(description = "请求方法")
    private String reqMethod;

    @Column(name = "status")
    @Schema(description = "状态 0-不可用 1-可用")
    private Integer status;

    @Column(name = "rest_name")
    @Schema(description = "所在类名")
    private String restName;

    @Column(name = "remark")
    @Schema(description = "备注")
    private String remark;

    public void copy(Resource source) {
        BeanUtil.copyProperties(source, this, CopyOptions.create().setIgnoreNullValue(true));
    }
}
