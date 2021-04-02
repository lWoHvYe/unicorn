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
package com.lwohvye.utils;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.annotation.DataPermission;
import com.lwohvye.annotation.Query;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.criteria.*;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * @author Zheng Jie
 * @date 2019-6-4 14:59:48
 */
@Slf4j
@SuppressWarnings({"unchecked", "all"})
public class QueryHelp {

    /**
     * @param root  Root根对象对应于from后面的表
     * @param query Q 外部的criteria对象
     * @param cb    CriteriaBuilder工厂类，用于创建查询的criteriaQuery对象
     *              Predicate查询条件的拼接对应于where后面的添加表达式
     * @return javax.persistence.criteria.Predicate
     * @description 解析属性上的查询注解。贫瘠相应的查询
     * @author Hongyan Wang
     * @date 2021/3/31 11:57
     */
    public static <R, Q> Predicate getPredicate(Root<R> root, Q query, CriteriaBuilder cb) {
        List<Predicate> list = new ArrayList<>();
        if (query == null) {
            return cb.and(list.toArray(new Predicate[0]));
        }
        // 数据权限验证
        DataPermission permission = query.getClass().getAnnotation(DataPermission.class);
        if (permission != null) {
            // 获取数据权限
            List<Long> dataScopes = SecurityUtils.getCurrentUserDataScope();
            if (CollectionUtil.isNotEmpty(dataScopes)) {
                if (StringUtils.isNotBlank(permission.joinName()) && StringUtils.isNotBlank(permission.fieldName())) {
                    Join join = root.join(permission.joinName(), JoinType.LEFT);
                    list.add(getExpression(permission.fieldName(), join, root).in(dataScopes));
                } else if (StringUtils.isBlank(permission.joinName()) && StringUtils.isNotBlank(permission.fieldName())) {
                    list.add(getExpression(permission.fieldName(), null, root).in(dataScopes));
                }
            }
        }
        try {
            List<Field> fields = getAllFields(query.getClass(), new ArrayList<>());
            for (Field field : fields) {
                boolean accessible = field.isAccessible();
                // 设置对象的访问权限，保证对private的属性的访
                field.setAccessible(true);
                Query q = field.getAnnotation(Query.class);
                if (q != null) {
                    String propName = q.propName();
                    String joinName = q.joinName();
                    String blurry = q.blurry();
                    String attributeName = isBlank(propName) ? field.getName() : propName;
                    Class<?> fieldType = field.getType();
                    Object val = field.get(query);
                    if (ObjectUtil.isNull(val) || "".equals(val)) {
                        continue;
                    }
                    Join join = null;
                    // 模糊多字段
                    if (ObjectUtil.isNotEmpty(blurry)) {
                        String[] blurrys = blurry.split(",");
                        List<Predicate> orPredicate = new ArrayList<>();
                        for (String s : blurrys) {
                            orPredicate.add(cb.like(root.get(s)
                                    .as(String.class), "%" + val.toString() + "%"));
                        }
                        Predicate[] p = new Predicate[orPredicate.size()];
                        list.add(cb.or(orPredicate.toArray(p)));
                        continue;
                    }
                    if (ObjectUtil.isNotEmpty(joinName)) {
                        String[] joinNames = joinName.split(">");
                        for (String name : joinNames) {
                            switch (q.join()) {
                                case LEFT:
                                    if (ObjectUtil.isNotNull(join) && ObjectUtil.isNotNull(val)) {
                                        join = join.join(name, JoinType.LEFT);
                                    } else {
                                        join = root.join(name, JoinType.LEFT);
                                    }
                                    break;
                                case RIGHT:
                                    if (ObjectUtil.isNotNull(join) && ObjectUtil.isNotNull(val)) {
                                        join = join.join(name, JoinType.RIGHT);
                                    } else {
                                        join = root.join(name, JoinType.RIGHT);
                                    }
                                    break;
                                case INNER:
                                    if (ObjectUtil.isNotNull(join) && ObjectUtil.isNotNull(val)) {
                                        join = join.join(name, JoinType.INNER);
                                    } else {
                                        join = root.join(name, JoinType.INNER);
                                    }
                                    break;
                                default:
                                    break;
                            }
                        }
                    }
                    switch (q.type()) {
                        case EQUAL:
                            list.add(cb.equal(getExpression(attributeName, join, root)
                                    .as((Class<? extends Comparable>) fieldType), val));
                            break;
                        case GREATER_THAN:
                            list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root)
                                    .as((Class<? extends Comparable>) fieldType), (Comparable) val));
                            break;
                        case LESS_THAN:
                            list.add(cb.lessThanOrEqualTo(getExpression(attributeName, join, root)
                                    .as((Class<? extends Comparable>) fieldType), (Comparable) val));
                            break;
                        case LESS_THAN_NQ:
                            list.add(cb.lessThan(getExpression(attributeName, join, root)
                                    .as((Class<? extends Comparable>) fieldType), (Comparable) val));
                            break;
                        case INNER_LIKE:
                            list.add(cb.like(getExpression(attributeName, join, root)
                                    .as(String.class), "%" + val.toString() + "%"));
                            break;
                        case LEFT_LIKE:
                            list.add(cb.like(getExpression(attributeName, join, root)
                                    .as(String.class), "%" + val.toString()));
                            break;
                        case RIGHT_LIKE:
                            list.add(cb.like(getExpression(attributeName, join, root)
                                    .as(String.class), val.toString() + "%"));
                            break;
                        case IN:
                            if (CollUtil.isNotEmpty((Collection<Long>) val)) {
                                list.add(getExpression(attributeName, join, root).in((Collection<Long>) val));
                            }
                            break;
                        case NOT_EQUAL:
                            list.add(cb.notEqual(getExpression(attributeName, join, root), val));
                            break;
                        case NOT_NULL:
                            list.add(cb.isNotNull(getExpression(attributeName, join, root)));
                            break;
                        case IS_NULL:
                            list.add(cb.isNull(getExpression(attributeName, join, root)));
                            break;
                        case BETWEEN:
                            List<Object> between = new ArrayList<>((List<Object>) val);
                            list.add(cb.between(getExpression(attributeName, join, root).as((Class<? extends Comparable>) between.get(0).getClass()),
                                    (Comparable) between.get(0), (Comparable) between.get(1)));
                            break;
                        case NOT_IN:
                            if (CollUtil.isNotEmpty((Collection<Long>) val)) {
                                list.add(cb.not(getExpression(attributeName, join, root).in((Collection<Long>) val)));
                            }
                            break;
                        case LIKE_STR:
                            list.add(cb.like(getExpression(attributeName, join, root)
                                    .as(String.class), val.toString()));
                            break;
                        case IN_OR_ISNULL:
                            if (CollUtil.isNotEmpty((Collection<Long>) val)) {
                                list.add(
//                                        在集合中
                                        cb.or(getExpression(attributeName, join, root).in((Collection<Long>) val)
//                                                或值为null
                                                , cb.isNull(getExpression(attributeName, join, root))
//                                                或值为空字符串
                                                , cb.equal(getExpression(attributeName, join, root).as(String.class), ""))
                                );
                            }
                            break;
                        case IS_OR_NULL:
                            list.add((Long) val == -1L ?
                                    cb.isNull(getExpression(attributeName, join, root).as((Class<? extends Comparable>) fieldType)) :
                                    cb.equal(getExpression(attributeName, join, root).as((Class<? extends Comparable>) fieldType), val));
                            break;
                        case IN_INNER_LIKE:
                            if (val instanceof List) {
                                var objList = (List) val;
//                                构建数组
                                var predicates = new Predicate[objList.size()];
                                for (int i = 0; i < objList.size(); i++) {
                                    var obj = objList.get(i);
                                    predicates[i] = cb.like(getExpression(attributeName, join, root)
                                            .as(String.class), "%" + obj.toString() + "%");
                                }
//                                设置or查询
                                list.add(cb.or(predicates));
                            }
                            break;
                        case EQUAL_IN_MULTI:
                            var predicates = new Predicate[4];
//                            like val
                            predicates[0] = cb.like(getExpression(attributeName, join, root).as(String.class), val.toString());
//                            like val,%
                            predicates[1] = cb.like(getExpression(attributeName, join, root).as(String.class), val.toString() + ",%");
//                            like %,val,%
                            predicates[2] = cb.like(getExpression(attributeName, join, root).as(String.class), "%," + val.toString() + ",%");
//                            like %,val
                            predicates[3] = cb.like(getExpression(attributeName, join, root).as(String.class), "%," + val.toString());
//                            设置查询
                            list.add(cb.or(predicates));
                            break;
                        case EQUAL_IN_MULTI_JOIN:
//                            该注解只针对Join查询。非join不处理
                            if (ObjectUtil.isNull(join))
                                break;
                            var arrayList = new ArrayList<Predicate>();
//                            val是一个实体。里面有多个属性。将其中非空的属性配置进去
                            for (Field fieldInVal : ReflectUtil.getFields(val.getClass())) {
                                var fieldValue = ReflectUtil.getFieldValue(val, fieldInVal);
                                if (ObjectUtil.isNotNull(fieldValue)) {
                                    Predicate predicate = null;
//                                    String类型使用Inner like
                                    if (fieldValue instanceof String)
                                        predicate = cb.like(getExpression(fieldInVal.getName(), join, root).as(String.class), "%" + fieldValue.toString() + "%");
//                                    传-1L时。做is null查询。因为long类型一般是主键类，不会为负值
                                        // TODO: 2021/4/2 当使用IS NULL查询时，同join的其他查询条件会导致无结果。故先只让该is null查询生效
                                    else if (fieldValue instanceof Long && ObjectUtil.equals(fieldValue, -1L)) {
                                        predicate = cb.isNull(getExpression(fieldInVal.getName(), join, root).as((Class<? extends Comparable>) fieldInVal.getType()));
                                        list.add(predicate);
//                                        安全起见。清空一下
                                        arrayList.clear();
                                        break;
                                    } else
//                                        其他的走等于
                                        predicate = cb.equal(getExpression(fieldInVal.getName(), join, root).as((Class<? extends Comparable>) fieldInVal.getType()), fieldValue);
                                    if (ObjectUtil.isNotNull(predicate))
                                        arrayList.add(predicate);
                                }
                            }
                            if (CollUtil.isNotEmpty(arrayList))
                                list.add(cb.and(arrayList.toArray(new Predicate[arrayList.size()])));
                            break;
                        default:
                            break;
                    }
                }
                field.setAccessible(accessible);
            }
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        int size = list.size();
        return cb.and(list.toArray(new Predicate[size]));
    }

    @SuppressWarnings("unchecked")
    private static <T, R> Expression<T> getExpression(String attributeName, Join join, Root<R> root) {
        if (ObjectUtil.isNotEmpty(join)) {
            return join.get(attributeName);
        } else {
            return root.get(attributeName);
        }
    }

    private static boolean isBlank(final CharSequence cs) {
        int strLen;
        if (cs == null || (strLen = cs.length()) == 0) {
            return true;
        }
        for (int i = 0; i < strLen; i++) {
            if (!Character.isWhitespace(cs.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static List<Field> getAllFields(Class clazz, List<Field> fields) {
        if (clazz != null) {
            fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
            getAllFields(clazz.getSuperclass(), fields);
        }
        return fields;
    }
}
