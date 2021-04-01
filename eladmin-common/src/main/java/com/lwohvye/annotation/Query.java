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
 * @author Zheng Jie
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

    enum Type {
        // jie 2019/6/4 相等
        EQUAL
        // Dong ZhaoYang 2017/8/7 大于等于
        , GREATER_THAN
        // Dong ZhaoYang 2017/8/7 小于等于
        , LESS_THAN
        // Dong ZhaoYang 2017/8/7 中模糊查询
        , INNER_LIKE
        // Dong ZhaoYang 2017/8/7 左模糊查询
        , LEFT_LIKE
        // Dong ZhaoYang 2017/8/7 右模糊查询
        , RIGHT_LIKE
        // Dong ZhaoYang 2017/8/7 小于
        , LESS_THAN_NQ
        // jie 2019/6/4 包含
        , IN
        // 不等于
        , NOT_EQUAL
        // between
        , BETWEEN
        // 不为空
        , NOT_NULL
        // 为空
        , IS_NULL
        // why 不在指定集合中，不建议使用。因为集合会很大，对效率影响较大
        , NOT_IN
        // why 两个分别用于自定义通配符的like
        , LIKE_STR
        // why 业务需要，在指定集合内或值为空
        , IN_OR_ISNULL
        // why 传非-1时，使用EQUAL,传-1时，使用IS_NULL。将not in 转为left join + 关联表id为null
        , IS_OR_NULL
        // why List的 IN模糊查询INNER_LIKE
        , IN_INNER_LIKE
        // why 使用逗号分割的多值中，某一个值的筛选
        , EQUAL_IN_MULTI
        // why 原连接查询都是单条件的。针对业务，多条件连接查询，QueryCriteria中使用一个实体来承载属性
        , EQUAL_IN_MULTI_JOIN
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

