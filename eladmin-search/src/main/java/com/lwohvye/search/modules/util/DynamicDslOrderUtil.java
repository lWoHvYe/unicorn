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

package com.lwohvye.search.modules.util;

import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.Expressions;

public class DynamicDslOrderUtil {

    /**
     * 根据入参动态排序
     *
     * @param pathBase  q实体
     * @param sortType  排序类型，升序ASC，降序DESC
     * @param sortField 排序字段名
     * @param <T>
     * @return
     */
    public static <T> OrderSpecifier<?> orderByField(EntityPathBase<T> pathBase, String sortType, String sortField) {
        var order = "asc".equalsIgnoreCase(sortType) ? Order.ASC : Order.DESC;
        var fieldPath = Expressions.path(Object.class, pathBase, sortField);
        return new OrderSpecifier(order, fieldPath);
    }
}
