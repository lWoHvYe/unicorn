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

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Objects;

@Entity
@Getter
@Setter
@ToString
@Accessors(chain = true)
@Table(name = "sys_user")
public class CustomizeUser implements Serializable {

    @Id
    @Column(name = "user_id")
    // 在jpa insert操作时，可以指定插入对主键id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true)
    private String username;

    private String email;

    private String phone;

    private String gender;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        var customizeUser = (CustomizeUser) o;
        return Objects.equals(id, customizeUser.id) &&
                Objects.equals(username, customizeUser.username);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, username);
    }
}
