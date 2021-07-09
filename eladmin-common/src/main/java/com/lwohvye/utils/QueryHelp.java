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

import javax.persistence.Id;
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
        var list = new ArrayList<Predicate>();
        if (query == null) {
            return cb.and(list.toArray(new Predicate[0]));
        }
        // 数据权限验证
        DataPermission permission = query.getClass().getAnnotation(DataPermission.class);
        if (permission != null) {
            // 获取数据权限
            var dataScopes = SecurityUtils.getCurrentUserDataScope();
            if (CollectionUtil.isNotEmpty(dataScopes)) {
                if (StringUtils.isNotBlank(permission.joinName()) && StringUtils.isNotBlank(permission.fieldName())) {
                    var join = root.join(permission.joinName(), JoinType.LEFT);
                    list.add(getExpression(permission.fieldName(), join, root).in(dataScopes));
                } else if (StringUtils.isBlank(permission.joinName()) && StringUtils.isNotBlank(permission.fieldName())) {
                    list.add(getExpression(permission.fieldName(), null, root).in(dataScopes));
                }
            }
        }
        try {
            var fields = getAllFields(query.getClass(), new ArrayList<>());
            for (var field : fields) {
//                field.canAccess(filed对应的查询器实例)
                var accessible = field.canAccess(query);
//                boolean accessible = field.isAccessible();
                // TODO: 2021/7/9 下面这两行，确认无误后，记得移除
                if (ObjectUtil.notEqual(accessible, field.isAccessible()))
                    throw new RuntimeException("编码有误" + field.toString() + accessible);
                // 设置对象的访问权限，保证对private的属性的访
                field.setAccessible(true);
                Query q = field.getAnnotation(Query.class);
                if (q != null) {
                    var propName = q.propName();
                    var joinName = q.joinName();
                    var blurry = q.blurry();
                    var attributeName = isBlank(propName) ? field.getName() : propName;
                    var fieldType = field.getType();
                    var val = field.get(query);
                    if (ObjectUtil.isNull(val) || "".equals(val)) {
                        continue;
                    }
                    Join join = null;
                    // 模糊多字段
                    if (ObjectUtil.isNotEmpty(blurry)) {
                        var blurrys = blurry.split(",");
                        var orPredicate = new ArrayList<Predicate>();
                        for (String s : blurrys) {
                            orPredicate.add(cb.like(root.get(s)
                                    .as(String.class), "%" + val.toString() + "%"));
                        }
                        var p = new Predicate[orPredicate.size()];
                        list.add(cb.or(orPredicate.toArray(p)));
                        continue;
                    }
//                    解析join类型
                    join = analyzeJoinType(root, q, joinName, val, join);
//                    解析查询类型
                    analyzeQueryType(root, cb, list, q, attributeName, (Class<? extends Comparable>) fieldType, val, join);
                }
                field.setAccessible(accessible);
            }
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        int size = list.size();
        return cb.and(list.toArray(new Predicate[size]));
    }

    /**
     * @param root
     * @param q
     * @param joinName
     * @param val
     * @param join
     * @return javax.persistence.criteria.Join
     * @description 解析joinType
     * @author Hongyan Wang
     * @date 2021/6/24 10:52 上午
     */
    private static <R> Join analyzeJoinType(Root<R> root, Query q, String joinName, Object val, Join join) {
        if (ObjectUtil.isNotEmpty(joinName)) {
            var joinNames = joinName.split(">");
            for (var name : joinNames) {
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
        return join;
    }

    /**
     * @param root
     * @param cb
     * @param list
     * @param q
     * @param attributeName
     * @param fieldType
     * @param val
     * @param join
     * @description 解析query.type()。抽取主要为了方便调用
     * @author Hongyan Wang
     * @date 2021/6/24 10:52 上午
     */
    private static <R> void analyzeQueryType(Root<R> root, CriteriaBuilder cb, ArrayList<Predicate> list, Query q, String attributeName, Class<? extends Comparable> fieldType, Object val, Join join) {
        switch (q.type()) {
            case EQUAL:
                list.add(cb.equal(getExpression(attributeName, join, root).as(fieldType), val));
                break;
            case GREATER_THAN:
                list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(fieldType), (Comparable) val));
                break;
            case LESS_THAN:
                list.add(cb.lessThanOrEqualTo(getExpression(attributeName, join, root).as(fieldType), (Comparable) val));
                break;
            case LESS_THAN_NQ:
                list.add(cb.lessThan(getExpression(attributeName, join, root).as(fieldType), (Comparable) val));
                break;
            case INNER_LIKE:
                list.add(cb.like(getExpression(attributeName, join, root).as(String.class), "%" + val.toString() + "%"));
                break;
            case LEFT_LIKE:
                list.add(cb.like(getExpression(attributeName, join, root).as(String.class), "%" + val.toString()));
                break;
            case RIGHT_LIKE:
                list.add(cb.like(getExpression(attributeName, join, root).as(String.class), val.toString() + "%"));
                break;
            case IN:
                if (CollUtil.isNotEmpty((Collection<? extends Comparable>) val)) {
                    list.add(getExpression(attributeName, join, root).in((Collection<? extends Comparable>) val));
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
                var between = new ArrayList<Object>((List<Object>) val);
                list.add(cb.between(getExpression(attributeName, join, root).as((Class<? extends Comparable>) between.get(0).getClass()), (Comparable) between.get(0), (Comparable) between.get(1)));
                break;
            case NOT_IN:
                if (CollUtil.isNotEmpty((Collection<? extends Comparable>) val)) {
                    list.add(cb.not(getExpression(attributeName, join, root).in((Collection<? extends Comparable>) val)));
                }
                break;
            case LIKE_STR:
                list.add(cb.like(getExpression(attributeName, join, root).as(String.class), val.toString()));
                break;
            case IN_OR_ISNULL:
                if (CollUtil.isNotEmpty((Collection<? extends Comparable>) val)) {
                    list.add(
//                                        在集合中
                            cb.or(getExpression(attributeName, join, root).in((Collection<? extends Comparable>) val)
//                                                或值为null
                                    , cb.isNull(getExpression(attributeName, join, root))
//                                                或值为空字符串
                                    , cb.equal(getExpression(attributeName, join, root).as(String.class), ""))
                    );
                }
                break;
            case IS_OR_NULL:
                list.add((Long) val == -1L ?
                        cb.isNull(getExpression(attributeName, join, root).as(fieldType)) :
                        cb.equal(getExpression(attributeName, join, root).as(fieldType), val));
                break;
            case IN_INNER_LIKE:
                if (val instanceof List objList) {
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
//                                    Id注解，只会出现在主键上
                        var idAnnotation = fieldInVal.getAnnotation(Id.class);
//                                    In查询通过Query注解的propName指定映射的属性
                        var queryAnnotation = fieldInVal.getAnnotation(Query.class);
//                                    如果在实体属性上配置了Query注解，需解析Query注解，确定查询方式
                        // TODO: 2021/6/24 当前只完成部分查询。
                        if (ObjectUtil.isNotNull(queryAnnotation)) {
                            var queryType = queryAnnotation.type();
                            var queryAttrName = isBlank(queryAnnotation.propName()) ? fieldInVal.getName() : queryAnnotation.propName();
                            switch (queryType) {
                                case EQUAL:
                                    predicate = cb.equal(getExpression(queryAttrName, join, root).as((Class<? extends Comparable>) fieldInVal.getType()), fieldValue);
                                    break;
                                case NOT_EQUAL:
                                    predicate = cb.notEqual(getExpression(queryAttrName, join, root), fieldValue);
                                    break;
                                case INNER_LIKE:
                                    predicate = cb.like(getExpression(queryAttrName, join, root).as(String.class), "%" + fieldValue.toString() + "%");
                                    break;
                                case IN:
                                    predicate = getExpression(queryAttrName, join, root).in((Collection<? extends Comparable>) fieldValue);
                                    break;
                                case NOT_IN:
                                    predicate = cb.not(getExpression(queryAttrName, join, root).in((Collection<? extends Comparable>) fieldValue));
                                    break;
                                default:
                                    throw new RuntimeException("暂不支持该类型，请期待后续支持：" + queryType);
                            }
//                                    String类型使用Inner like
                        } else if (fieldValue instanceof String) {
                            predicate = cb.like(getExpression(fieldInVal.getName(), join, root).as(String.class), "%" + fieldValue.toString() + "%");
//                                    传-1L时。做is null查询。额外限制为当对应属性上有id注解的时候。
                            // TODO: 2021/4/2 当使用IS NULL查询时，同join的其他查询条件会导致无结果。故先只让该is null查询生效。
                            // TODO: 2021/4/6 经考虑，IS NULL类查询更建议使用其他的方式来完成。 EQUAL_IN_MULTI_JOIN注解主要用在多条件join上（不包括is null）
                            // TODO: 2021/4/6 针对与is null的需求，可以考虑视图。这种一般不需要太多张表。后续会探讨如何将join的相关筛选放在on 后面
                        } else if (fieldValue instanceof Long && ObjectUtil.equals(fieldValue, -1L) && ObjectUtil.isNotNull(idAnnotation)) {
                            predicate = cb.isNull(getExpression(fieldInVal.getName(), join, root).as((Class<? extends Comparable>) fieldInVal.getType()));
                            list.add(predicate);
//                                        安全起见。清空一下
                            arrayList.clear();
                            break;
                        } else {
//                                        其他的走等于
                            predicate = cb.equal(getExpression(fieldInVal.getName(), join, root).as((Class<? extends Comparable>) fieldInVal.getType()), fieldValue);
                        }
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
