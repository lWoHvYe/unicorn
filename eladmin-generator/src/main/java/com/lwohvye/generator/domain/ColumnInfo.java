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
package com.lwohvye.generator.domain;

import com.lwohvye.generator.utils.GenUtil;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;

/**
 * 列的数据信息
 * @author Zheng Jie
 * @date 2019-01-02
 */
@Getter
@Setter
@Entity
@NoArgsConstructor
@Table(name = "code_column_config")
public class ColumnInfo implements Serializable {

    @Id
    @Column(name = "column_id")
    @Schema(description = "ID" , accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Schema(description = "表名" )
    private String tableName;

    @Schema(description = "数据库字段名称" )
    private String columnName;

    @Schema(description = "数据库字段类型" )
    private String columnType;

    @Schema(description = "数据库字段键类型" )
    private String keyType;

    @Schema(description = "字段额外的参数" )
    private String extra;

    @Schema(description = "数据库字段描述" )
    private String remark;

    @Schema(description = "是否必填" )
    private Boolean notNull;

    @Schema(description = "是否在列表显示" )
    private Boolean listShow;

    @Schema(description = "是否表单显示" )
    private Boolean formShow;

    @Schema(description = "表单类型" )
    private String formType;

    @Schema(description ="查询 1:模糊 2：精确")
    private String queryType;

    @Schema(description = "字典名称" )
    private String dictName;

    @Schema(description = "日期注解" )
    private String dateAnnotation;

    public ColumnInfo(String tableName, String columnName, Boolean notNull, String columnType, String remark, String keyType, String extra) {
        this.tableName = tableName;
        this.columnName = columnName;
        this.columnType = columnType;
        this.keyType = keyType;
        this.extra = extra;
        this.notNull = notNull;
        if(GenUtil.PK.equalsIgnoreCase(keyType) && GenUtil.EXTRA.equalsIgnoreCase(extra)){
            this.notNull = false;
        }
        this.remark = remark;
        this.listShow = true;
        this.formShow = true;
    }
}
