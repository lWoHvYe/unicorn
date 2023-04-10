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
package sample.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * 角色
 *
 * @author Zheng Jie
 * @date 2018-11-22
 */
@Entity
@Getter
@Setter
@Accessors(chain = true)
@Table(name = "sys_role")
public class CustomizeRole implements Serializable {

    @Id
    @Column(name = "role_id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @JsonIgnore
    @ManyToMany(mappedBy = "customizeRoles")
    private Set<CustomizeUser> customizeUsers;

    private String name;

    private String code;

    @Column(name = "level")
    private Integer level = 3;

    private String description;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        CustomizeRole customizeRole = (CustomizeRole) o;
        return Objects.equals(id, customizeRole.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
