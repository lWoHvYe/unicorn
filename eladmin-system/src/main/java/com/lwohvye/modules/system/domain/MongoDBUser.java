package com.lwohvye.modules.system.domain;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.io.Serializable;

@Document(collection = "mongoDBUser")
@Getter
@Setter
@ToString
public class MongoDBUser implements Serializable {

    private static final long serialVersionUID = -3258839839160856613L;

    @Id
    private Long id;

    /**
     * 声明username加索引，加快查询速度
     */
    @Indexed
    private String userName;

    @Field("pwd")
    private String passWord;

}
