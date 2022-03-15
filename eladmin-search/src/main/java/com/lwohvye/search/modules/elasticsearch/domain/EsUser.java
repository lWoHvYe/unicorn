/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.lwohvye.search.modules.elasticsearch.domain;

import lombok.*;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.io.Serializable;

// https://docs.spring.io/spring-data/elasticsearch/docs/4.2.5/reference/html/
//@Document 注解主要声明索引名、类型名、分片数量和备份数量
//@Field 注解主要声明字段对应ES的类型
@Document(indexName = "she_es_user")
@Getter
@Setter
@ToString
@Accessors(chain = true)
public class EsUser implements Serializable {
    /*
    The following annotations are available:

    @Document: Applied at the class level to indicate this class is a candidate for mapping to the database. The most important attributes are:
        indexName: the name of the index to store this entity in. This can contain a SpEL template expression like "log-#{T(java.time.LocalDate).now().toString()}"
        type: the mapping type. If not set, the lowercased simple name of the class is used. (deprecated since version 4.0)
        createIndex: flag whether to create an index on repository bootstrapping. Default value is true. See Automatic creation of indices with the corresponding mapping
        versionType: Configuration of version management. Default value is EXTERNAL.
    @Id: Applied at the field level to mark the field used for identity purpose.
    @Transient: By default all fields are mapped to the document when it is stored or retrieved, this annotation excludes the field.
    @PersistenceConstructor: Marks a given constructor - even a package protected one - to use when instantiating the object from the database. Constructor arguments are mapped by name to the key values in the retrieved Document.
    @Field: Applied at the field level and defines properties of the field, most of the attributes map to the respective Elasticsearch Mapping definitions (the following list is not complete, check the annotation Javadoc for a complete reference):
        name: The name of the field as it will be represented in the Elasticsearch document, if not set, the Java field name is used.
        type: The field type, can be one of Text, Keyword, Long, Integer, Short, Byte, Double, Float, Half_Float, Scaled_Float, Date, Date_Nanos, Boolean, Binary, Integer_Range, Float_Range, Long_Range, Double_Range, Date_Range, Ip_Range, Object, Nested, Ip, TokenCount, Percolator, Flattened, Search_As_You_Type. See Elasticsearch Mapping Types
        format: One or more built-in date formats, see the next section Date format mapping.
        pattern: One or more custom date formats, see the next section Date format mapping.
        store: Flag whether the original field value should be store in Elasticsearch, default value is false.
        analyzer, searchAnalyzer, normalizer for specifying custom analyzers and normalizer.
    @GeoPoint: Marks a field as geo_point datatype. Can be omitted if the field is an instance of the GeoPoint class.
     */
    // 主键
    // 	Please note that the id property needs to be of type String
    @Id
    private String id;

    @Field(type = FieldType.Keyword)
    private String userName;

    @Field(type = FieldType.Text)
    private String passWord;

    @Field(type = FieldType.Keyword)
    private String roleName;

    @Field(type = FieldType.Integer)
    private Integer sex;

}
