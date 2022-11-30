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
package com.lwohvye.api.modules.system.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.lwohvye.core.base.BaseEntity;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-12-17
 */
@Entity
@Getter
@Setter
@Accessors(chain = true)
@Table(name = "sys_menu")
public class Menu extends BaseEntity implements Serializable {

    @Id
    @Column(name = "menu_id")
    @NotNull(groups = {Update.class})
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @JsonIgnore
    @ManyToMany(mappedBy = "menus")
    @Schema(description = "菜单角色")
    private Set<Role> roles;

    @Schema(description = "菜单标题")
    private String title;

    @Column(name = "name")
    @Schema(description = "菜单组件名称")
    private String componentName;

    @Schema(description = "排序")
    private Integer menuSort = 999;

    @Schema(description = "组件路径")
    private String component;

    @Schema(description = "路由地址")
    private String path;

    @Schema(description = "菜单类型，目录、菜单、按钮")
    private Integer type;

    // @Schema(description = "权限标识" )
    // private String permission;

    @Schema(description = "菜单图标")
    private String icon;

    @Column(columnDefinition = "bit(1) default 0")
    @Schema(description = "缓存")
    private Boolean cache;

    @Column(columnDefinition = "bit(1) default 0")
    @Schema(description = "是否隐藏")
    private Boolean hidden;

    @Schema(description = "上级菜单")
    private Long pid;

    @Schema(description = "子节点数目", accessMode = Schema.AccessMode.READ_ONLY)
    private Integer subCount = 0;

    @Schema(description = "外链菜单")
    private Boolean iFrame;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Menu menu = (Menu) o;
        return Objects.equals(id, menu.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
