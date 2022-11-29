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
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.util.List;

/**
 * @author Zheng Jie
 * @date 2019-04-10
 */
@Entity
@Getter
@Setter
@Accessors(chain = true)
@Table(name = "sys_dict")
public class Dict extends BaseEntity implements Serializable {

    @Id
    @Column(name = "dict_id")
    @NotNull(groups = Update.class)
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @JsonIgnore
    @OneToMany(mappedBy = "dict", cascade = {CascadeType.PERSIST, CascadeType.REMOVE}) // 配置这个主体就是为了级联删除
    private List<DictDetail> dictDetails; // 用transient出现了级联删除失效，改用@JsonIgnore

    @NotBlank
    @Schema(description = "名称")
    private String name;

    @Schema(description = "描述")
    private String description;
}
