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
package com.lwohvye.modules.system.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.lwohvye.base.BaseEntity;
import com.lwohvye.utils.enums.DataScopeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * 角色
 *
 * @author Zheng Jie
 * @date 2018-11-22
 */
@NamedEntityGraph(name = "Role-Details", attributeNodes = {@NamedAttributeNode("menus"), @NamedAttributeNode("depts"), @NamedAttributeNode("resources")})
@Entity
@Getter
@Setter
@Accessors(chain = true)
@Table(name = "sys_role")
public class Role extends BaseEntity implements Serializable {

    @Id
    @Column(name = "role_id")
    @NotNull(groups = {Update.class})
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;

    @JsonIgnore
    @ManyToMany(mappedBy = "roles")
    @Schema(description = "用户", accessMode = Schema.AccessMode.READ_ONLY)
    private Set<User> users;

    @ManyToMany
    @JoinTable(name = "sys_roles_menus",
            joinColumns = {@JoinColumn(name = "role_id", referencedColumnName = "role_id")},
            inverseJoinColumns = {@JoinColumn(name = "menu_id", referencedColumnName = "menu_id")})
    @Schema(description = "菜单", accessMode = Schema.AccessMode.READ_ONLY)
    private Set<Menu> menus;

    @ManyToMany
    @JoinTable(name = "sys_roles_depts",
            joinColumns = {@JoinColumn(name = "role_id", referencedColumnName = "role_id")},
            inverseJoinColumns = {@JoinColumn(name = "dept_id", referencedColumnName = "dept_id")})
    @Schema(description = "部门", accessMode = Schema.AccessMode.READ_ONLY)
    private Set<Dept> depts;

    @ManyToMany(cascade = {CascadeType.PERSIST}, fetch = FetchType.LAZY)
    @JoinTable(name = "sys_roles_resources",
            joinColumns = {@JoinColumn(name = "role_id", referencedColumnName = "role_id")},
            inverseJoinColumns = {@JoinColumn(name = "resource_id", referencedColumnName = "resource_id")})
    @Schema(description = "资源", accessMode = Schema.AccessMode.READ_ONLY)
    private Set<Resource> resources;

    @NotBlank
    @Schema(description = "名称", accessMode = Schema.AccessMode.READ_ONLY)
    private String name;

    @NotBlank
    @Schema(description = "标识", accessMode = Schema.AccessMode.READ_ONLY)
    private String code;

    @Schema(description = "数据权限，全部 、 本级 、 自定义")
    private String dataScope = DataScopeEnum.THIS_LEVEL.getValue();

    @Column(name = "level")
    @Schema(description = "级别，数值越小，级别越大")
    private Integer level = 3;

    @Schema(description = "描述")
    private String description;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Role role = (Role) o;
        return Objects.equals(id, role.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
