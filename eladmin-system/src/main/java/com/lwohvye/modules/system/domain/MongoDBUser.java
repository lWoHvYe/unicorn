package com.lwohvye.modules.system.domain;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.io.Serializable;

@Document
@Getter
@Setter
@ToString
@Accessors(chain = true)
public class MongoDBUser implements Serializable {

    @Id
    private String id;

    /**
     * 声明username加索引，加快查询速度
     */
    @Indexed
    private String userName;

    private String passWord;

    //    角色
    private String roleName;

}
