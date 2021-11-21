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

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.lwohvye.annotation.String4Blob;
import com.lwohvye.base.BaseEntity;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
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
@NamedEntityGraph(name = "User-Details", attributeNodes = {@NamedAttributeNode("roles"), @NamedAttributeNode("jobs"), @NamedAttributeNode("dept")})
@Entity
@Getter
@Setter
@Accessors(chain = true)
//@DynamicInsert属性:默认为true,表示insert对象的时候,生成动态的insert语句,如果这个字段的值是null就不会加入到insert语句当中。
// 但需注意jpa的save方法返回的实体是不包含那些数据库中配置的默认值的，因为默认值是在整个事务提交时才生的效，所以返回（包含事务内再查询），都不会生效，这个需特别注意。
@DynamicInsert
//@DynamicUpdate属性:默认为true,表示update对象的时候,生成动态的update语句,如果这个字段的值是null就不会被加入到update语句中。这个也许不太常用
//@DynamicUpdate
@Table(name = "#{local.sys.table-name.user:sys_user}")
public class User extends BaseEntity implements Serializable {

    @Id
    @Column(name = "user_id")
    @NotNull(groups = Update.class)
    // 在jpa insert操作时，可以指定插入对主键id
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "lid")
    @GenericGenerator(name = "lid", strategy = "com.lwohvye.config.LocalInsertGenerator")
//    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Schema(description = "ID" , accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;

    @ManyToMany
    @Schema(description = "用户角色" )
    @JoinTable(name = "sys_users_roles",
            joinColumns = {@JoinColumn(name = "user_id", referencedColumnName = "user_id")},
            inverseJoinColumns = {@JoinColumn(name = "role_id", referencedColumnName = "role_id")})
    private Set<Role> roles;

    @ManyToMany
    @Schema(description = "用户岗位" )
    @JoinTable(name = "sys_users_jobs",
            joinColumns = {@JoinColumn(name = "user_id", referencedColumnName = "user_id")},
            inverseJoinColumns = {@JoinColumn(name = "job_id", referencedColumnName = "job_id")})
    private Set<Job> jobs;

    @OneToOne
    @JoinColumn(name = "dept_id")
    @Schema(description = "用户部门" )
    private Dept dept;

    // @JsonAlias注解需要依赖于setter、getter，而@JsonProperty注解不需要
    @NotBlank
    @Column(unique = true)
    @Schema(description = "用户名称" )
    // @JsonAlias注解，实现:json转模型时，使json中的特定key能转化为特定的模型属性
    // 但是模型转json时，对应的转换后的key仍然与属性名一致
    @JsonAlias(value = {"userName", "uName"})
    private String username;

    @NotBlank
    @Schema(description = "用户昵称" )
    // @JsonProperty注解，实现：json转模型时，使json中的特定key能转化为指定的模型属性；
    // 同样的，模型转json时，对应的转换后的key为指定的key
    @JsonProperty(value = "nickName")
    private String nickName;

    @Email
    @NotBlank
    @Schema(description = "邮箱" )
    private String email;

    @NotBlank
    @Schema(description = "电话号码" )
    private String phone;

    @Schema(description = "用户性别" )
    private String gender;

    @Schema(description = "头像真实名称" , accessMode = Schema.AccessMode.READ_ONLY)
    private String avatarName;

    @Schema(description = "头像存储的路径" , accessMode = Schema.AccessMode.READ_ONLY)
    private String avatarPath;

    @Schema(description = "密码" )
    private String password;

    @NotNull
    @Schema(description = "是否启用" )
    private Boolean enabled;

    @Schema(description = "是否为admin账号" , accessMode = Schema.AccessMode.READ_ONLY)
    private Boolean isAdmin = false;

    // 描述信息。数据库中为blob类型。java侧使用String存取，存在乱码问题。在Mysql8的版本已无该问题。
    // 如果只是文本的存储，建议数据库使用text，当需要存储图片或视频等时，再考虑blob（二进制存储）。text无需考虑编码问题
    @String4Blob
    @Column(name = "description")
    private String description;

    @Column(name = "pwd_reset_time")
    @Schema(description = "最后修改密码的时间" , accessMode = Schema.AccessMode.READ_ONLY)
    private Date pwdResetTime;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        User user = (User) o;
        return Objects.equals(id, user.id) &&
               Objects.equals(username, user.username);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, username);
    }
}
