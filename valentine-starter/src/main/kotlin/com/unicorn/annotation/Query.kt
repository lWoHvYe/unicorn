/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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

package com.unicorn.annotation

/**
 * 查询注解，针对查询的main-table,
 * field `propName` define the queryFieldName (can be a field in the joined entity, will use the filed name if null)
 * field `type` define the queryType, equal,like,greater_than,less_than,...
 * field `join` define the table-join-type, left,right,inner, tips: will use the fields defined in the @JoinTable/@JoinColum to do join
 * field `joinName` define the joined-table-name in the main-entity (can't be null for join)
 *
 * @author Zheng Jie, lWoHvYe
 * @date 2019-6-4 13:52:30
 */
@Target(AnnotationTarget.FIELD)
@Retention(AnnotationRetention.RUNTIME)
annotation class Query(
    /**
     * 查询对象的属性名，可以是main-entity中的，也可以是joined-entity中的，为null时使用filedName
     */
    val propName: String = "",
    /**
     * 查询方式，默认equal
     */
    val type: Type = Type.EQUAL,
    /**
     * 连接查询的属性名，如User类中的dept，
     * 支持嵌套，比如要根据Role中Menu的title来查User，可set该field为 roles>menus，并将propName set为title即可，这个可预见的有不少使用场景
     * 连接查询中不可为null，使用@JoinTable/@JoinColum中配置的属性进行tableJoin
     */
    val joinName: String = "",
    /**
     * 连接类型，默认左连接
     */
    val join: Join = Join.LEFT,
    /**
     * 多字段模糊搜索，仅支持String类型字段，多个用逗号隔开, 如@Query(blurry = "email,username")
     */
    val blurry: String = "",
    /**
     * 库函数名，做库函数调用，注意其使用方式(要对调用结果做上层逻辑)
     */
    val functionName: String = ""
) {
    enum class Type {
        // jie 2019/6/4 相等
        EQUAL,
        NOT_EQUAL,
        GREATER_THAN,
        LESS_THAN,
        LESS_THAN_NQ,
        INNER_LIKE,
        LEFT_LIKE,
        RIGHT_LIKE,
        LIKE_STR,
        IN_INNER_LIKE,
        IN,
        NOT_IN,
        BETWEEN,
        NOT_NULL,
        IS_NULL,
        EQUAL_IN_MULTI,
        FUNCTION_FIND_IN_SET,
        FUNCTION_4_EQUAL
    }

    /**
     * @author Zheng Jie
     * 适用于简单连接查询，复杂的请自定义该注解，或者使用sql查询
     */
    enum class Join {
        /**
         * jie 2019-6-4 13:18:30
         */
        LEFT,
        RIGHT,
        INNER
    }
}
