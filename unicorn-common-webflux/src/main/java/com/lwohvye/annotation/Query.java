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
package com.lwohvye.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Zheng Jie, lWoHvYe
 * @date 2019-6-4 13:52:30
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Query {

    // Dong ZhaoYang 2017/8/7 基本对象的属性名
    String propName() default "";

    // Dong ZhaoYang 2017/8/7 查询方式
    Type type() default Type.EQUAL;

    /**
     * 连接查询的属性名，如User类中的dept
     */
    String joinName() default "";

    /**
     * 默认左连接
     */
    Join join() default Join.LEFT;

    /**
     * 多字段模糊搜索，仅支持String类型字段，多个用逗号隔开, 如@Query(blurry = "email,username")
     */
    String blurry() default "";

    // 库函数名
    String functionName() default "";

    enum Type {
        // jie 2019/6/4 相等
        EQUAL
        // 不等于
        , NOT_EQUAL
        // Dong ZhaoYang 2017/8/7 大于等于
        , GREATER_THAN
        // Dong ZhaoYang 2017/8/7 小于等于
        , LESS_THAN
        // Dong ZhaoYang 2017/8/7 小于
        , LESS_THAN_NQ
        // Dong ZhaoYang 2017/8/7 中模糊查询
        , INNER_LIKE
        // Dong ZhaoYang 2017/8/7 左模糊查询
        , LEFT_LIKE
        // Dong ZhaoYang 2017/8/7 右模糊查询
        , RIGHT_LIKE
        // why 两个分别用于自定义通配符的like
        , LIKE_STR
        // why List的 IN模糊查询INNER_LIKE
        , IN_INNER_LIKE
        // jie 2019/6/4 包含
        , IN
        // why 不在指定集合中，不建议使用。因为集合会很大，对效率影响较大
        , NOT_IN
        // between
        , BETWEEN
        // 不为空
        , NOT_NULL
        // 为空
        , IS_NULL
        // why 业务需要，在指定集合内或值为空
        , IN_OR_ISNULL
        // why 传非-1时，使用EQUAL,传-1时，使用IS_NULL。将not in 转为left join + 关联表id为null。只支持数值类型
        , IS_OR_NULL
        // why 使用逗号分割的多值中，某一个值的筛选
        , EQUAL_IN_MULTI
        // why 这个看起来比上面的 EQUAL_IN_MULTI 要优雅一些，且需注意上面这个用的or查询，若对应列无索引会导致所有的索引失效（用 and 连接的其他条件也不走索引,另外有双端模糊不清楚影响情况），而这个因为用了库函数，该列的索引是失效的，所以看情况选择吧
        , FUNCTION_FIND_IN_SET
        // why 原连接查询都是单条件的。针对业务，多条件连接查询，QueryCriteria中使用一个实体来承载属性
        , @Deprecated(since = "3.10") EQUAL_IN_MULTI_JOIN //2022-07-28 这个的设计，主要是当初无法解决对同一个entity配置多个join就join多次的问题，当前已解决，所以deprecated该查询方式
        // why 2021/11/07 from_base64函数。当前函数只能对属性使用，不能对值使用
        // , FUNCTION_FROM_BASE64
        // why 2021/11/07 库函数，做相等查询
        , FUNCTION_4_EQUAL
    }

    /**
     * @author Zheng Jie
     * 适用于简单连接查询，复杂的请自定义该注解，或者使用sql查询
     */
    enum Join {
        /**
         * jie 2019-6-4 13:18:30
         */
        LEFT, RIGHT, INNER
    }

}

