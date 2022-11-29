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
package com.lwohvye.search.modules.sp.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import jakarta.persistence.*;
import java.io.Serializable;

/**
 * @author Zheng Jie
 * @date 2018-12-17
 */
@Entity
@Getter
@Setter
@Accessors(chain = true)
@Table(name = "sys_menu_view")
public class SimMenu implements Serializable {

    @Id
    @Column(name = "menu_id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String title;

    @Column(name = "name")
    private String componentName;

    private Integer type;

    private Long pid;

    // 设置Ignore，也许就能打破循环依赖了
    @JsonIgnore
    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "role_id")
    private SimRole simRole;

    // 改下toString总行了吧。剔除simRole
    @Override
    public String toString() {
        Long var10000 = this.getId();
        return "SimMenu(id=" + var10000 + ", title=" + this.getTitle() + ", componentName=" + this.getComponentName() + ", type=" + this.getType() + ", pid=" + this.getPid() + ")";
    }
}
