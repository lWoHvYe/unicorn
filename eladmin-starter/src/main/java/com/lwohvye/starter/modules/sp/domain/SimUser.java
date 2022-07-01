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
package com.lwohvye.starter.modules.sp.domain;

import com.fasterxml.jackson.annotation.JsonAlias;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-11-22
 */
@Entity
@Getter
@Setter
@ToString
@Accessors(chain = true)
//@DynamicInsert属性:默认为true,表示insert对象的时候,生成动态的insert语句,如果这个字段的值是null就不会加入到insert语句当中。
// 但需注意jpa的save方法返回的实体是不包含那些数据库中配置的默认值的，因为默认值是在整个事务提交时才生的效，所以返回（包含事务内再查询），都不会生效，这个需特别注意。
@DynamicInsert
//@DynamicUpdate属性:默认为true,表示update对象的时候,生成动态的update语句,如果这个字段的值是null就不会被加入到update语句中。这个也许不太常用
//@DynamicUpdate
@Table(name = "sys_user")
public class SimUser implements Serializable {

    @Id
    @Column(name = "user_id")
    // 在jpa insert操作时，可以指定插入对主键id
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "lid")
    @GenericGenerator(name = "lid", strategy = "com.lwohvye.sys.config.common.LocalInsertGenerator")
//    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Transient
    private List<SimRole> simRoles;

    // @JsonAlias注解需要依赖于setter、getter，而@JsonProperty注解不需要
    @Column(unique = true)
    // @JsonAlias注解，实现:json转模型时，使json中的特定key能转化为特定的模型属性
    // 但是模型转json时，对应的转换后的key仍然与属性名一致
    @JsonAlias(value = {"userName", "uName"})
    private String username;

    private String phone;

    private String password;

    private Boolean enabled;

}
