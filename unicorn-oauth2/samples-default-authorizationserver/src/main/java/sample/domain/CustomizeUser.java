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

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.Objects;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-11-22
 */
// 使用@NamedEntityGraph解决Jpa懒加载经典的 N + 1 问题
// https://docs.oracle.com/javaee/7/tutorial/persistence-entitygraphs002.htm?utm_source=product&utm_medium=link&utm_campaign=IU&utm_content=2021.2#
// A sub graph is basically an entity graph that is embedded into another entity graph or entity sub graph. The definition of a sub graph is similar to the definition of an entity graph.
// 还可以配置subgraphs，用于在本实体中，配置视图中属性，如roles中属性的视图。当前未用到这部分
@NamedEntityGraph(name = "User-Details", attributeNodes = {@NamedAttributeNode("customizeRoles")})
@Entity
@Getter
@Setter
@Accessors(chain = true)
@Table(name = "sys_user")
public class CustomizeUser implements Serializable {

    @Id
    @Column(name = "user_id")
    // 在jpa insert操作时，可以指定插入对主键id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
//    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToMany
    // 关于多对多关系的两种配置方式，可以参考另一个项目，当采用多对多的方式时，没有中间的关联层模型，比较简单，但更新时关联关系会采用先清空再重新添加的方式。而若采用一对多的方式，虽然复杂一点，但因为中间层有id，会自动的根据比较结果删除、更新或新增
    @JoinTable(name = "sys_users_roles",
            joinColumns = {@JoinColumn(name = "user_id", referencedColumnName = "user_id")},
            inverseJoinColumns = {@JoinColumn(name = "role_id", referencedColumnName = "role_id")})
    private Set<CustomizeRole> customizeRoles;

    // @JsonAlias注解需要依赖于setter、getter，而@JsonProperty注解不需要
    @Column(unique = true)
    // @JsonAlias注解，实现:json转模型时，使json中的特定key能转化为特定的模型属性
    // 但是模型转json时，对应的转换后的key仍然与属性名一致
    @JsonAlias(value = {"userName", "uName"})
    private String username;

    // @JsonProperty注解，实现：json转模型时，使json中的特定key能转化为指定的模型属性；
    // 同样的，模型转json时，对应的转换后的key为指定的key
    @JsonProperty(value = "nickName")
    private String nickName;

    private String email;

    private String phone;

    private String gender;

    private String avatarName;

    private String avatarPath;

    private String password;

    private Boolean enabled;

    private Boolean isAdmin = false;

    @Column(name = "pwd_reset_time")
    private Date pwdResetTime;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        CustomizeUser customizeUser = (CustomizeUser) o;
        return Objects.equals(id, customizeUser.id) &&
                Objects.equals(username, customizeUser.username);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, username);
    }
}
