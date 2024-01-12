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

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.io.Serializable;
import java.util.Objects;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@Table(name = "sys_user")
public class CustomizeUser implements Serializable {

    @Id
    private Long userId;

    // 当属性是private时，反序列化时，将根据get/set方法设置属性的值，所以若属性命名不规范（比如首字母大写Username，即便json中也是Username，但setUsername对应的属性应为username，所以绑定不上），
    // 就可能绑定不上，这时可用该注解来定义。当然也可将属性定义成public的，这时应可直接获得属性的名称，所以可以绑定上，但极度不推荐
    @JsonProperty("UserName")
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
        CustomizeUser customizeUser = (CustomizeUser) o;
        return Objects.equals(userId, customizeUser.userId) &&
                Objects.equals(username, customizeUser.username);
    }

    @Override
    public int hashCode() {
        return Objects.hash(userId, username);
    }
}
