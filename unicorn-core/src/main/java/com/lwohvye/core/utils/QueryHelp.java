/*
 *  Copyright 2019-2022 Zheng Jie
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
package com.lwohvye.core.utils;

import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.core.annotation.Query;
import com.lwohvye.core.exception.UtilsException;
import com.lwohvye.core.extension.AnyExtensionMethod;
import jakarta.persistence.criteria.*;
import lombok.experimental.ExtensionMethod;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * A utility class for parsing and generating query predicates based on query annotations on fields.
 * <p>
 * This class provides methods for analyzing and generating query predicates based on query annotations
 * on fields of a given query object. It supports simple multiple condition and join queries, but does
 * not support complex And, Or combination queries. For complex queries, it is recommended to use
 * QueryDSL or Native SQL.
 *
 * @author Zheng Jie, lWoHvYe
 * @since 2.6.0
 */
@Slf4j
@UtilityClass
@ExtensionMethod({AnyExtensionMethod.class})
// @SuppressWarnings 抑制警告 https://www.lwohvye.com/2021/12/05/suppresswarnings%e6%b3%a8%e8%a7%a3%e7%94%a8%e6%b3%95/
// @SuppressWarnings({"unchecked", "rawtypes", "unused"})
public class QueryHelp {

    /**
     * 每个 criteria 类型的 @Query 字段元数据只解析一次，避免每次查询都做字段遍历/注解读取/accessible 切换。
     */
    private static final ClassValue<List<QueryField>> QUERY_FIELDS = new ClassValue<>() {
        @Override
        protected List<QueryField> computeValue(Class<?> type) {
            var fields = new ArrayList<QueryField>();
            for (var field : ReflectUtil.getFields(type)) {
                var query = field.getAnnotation(Query.class);
                if (query == null) {
                    continue;
                }
                if (!field.trySetAccessible()) {
                    // 命名模块下如果没有把所在包 opens 给 lwohvye.unicorn.core，条件会被静默忽略。
                    throw new UtilsException("无法访问 @Query 字段: " + type.getName() + "#" + field.getName()
                            + "，请将所在包 opens 给 lwohvye.unicorn.core");
                }
                fields.add(new QueryField(field, query));
            }
            return List.copyOf(fields);
        }
    };

    private record QueryField(Field field, Query query) {
    }

    /**
     * 解析属性上的查询注解。贫瘠相应的查询
     * 当前已经支持了简单多条件的连表查询，但无法支持复杂的 And, Or 组合查询，这种要么使用QueryDSL要么干脆使用Native SQL。
     * 虽说通过整合Annotation和Reflect 也是可以支持复杂查询的，但既然已经有QueryDSL（虽然用的不多且好久没更新了），个人认为没必要重复造轮子，尤其是JPA跟其还是兼容的
     *
     * @param root  Root根对象对应于from后面的表
     * @param query Q 外部的criteria对象
     * @param cb    CriteriaBuilder工厂类，用于创建查询的criteriaQuery对象
     *              Predicate查询条件的拼接对应于where后面的添加表达式
     * @return jakarta.persistence.criteria.Predicate
     * @date 2021/3/31 11:57
     */
    public static <R, Q> Predicate getPredicate(Root<R> root, Q query, CriteriaBuilder cb) {
        var list = new ArrayList<Predicate>();
        if (query == null) {
            return cb.and(list.toArray(new Predicate[0]));
        }
        try {
            // 根据Field及上面的Annotation，拼接Query & Join。joins 只在本次查询内复用。
            var joins = new HashMap<String, Join<R, ?>>();
            analyzeFieldQuery(root, query, cb, list, joins);
        } catch (UtilsException e) {
            log.error("Fail to analyze query: " + query.getClass().getName(), e);
            throw e;
        } catch (Exception e) {
            log.error("Fail to analyze query: " + query.getClass().getName(), e);
            throw new UtilsException("解析查询失败: " + query.getClass().getName(), e);
        }
        // 各Field的Condition通过And连接，这是比较常见的场景
        return cb.and(list.toArray(new Predicate[0]));
    }

    private static <R, Q> void analyzeFieldQuery(Root<R> root, Q query, CriteriaBuilder cb,
                                                 ArrayList<Predicate> list,
                                                 Map<String, Join<R, ?>> joins) {
        for (var meta : QUERY_FIELDS.get(query.getClass())) {
            try {
                var field = meta.field();
                var q = meta.query();
                var value = field.get(query);
                var nullCondition = q.type() == Query.Type.IS_NULL || q.type() == Query.Type.NOT_NULL;
                // IS_NULL/NOT_NULL 使用 Boolean 字段作为开关：true 才追加条件，
                // null/false 表示“本次不限制”。不能因为字段为 null 就无条件追加，
                // 否则 MenuQueryCriteria 里 pidIsNull 被置空后仍会生成 pid is null。
                if (nullCondition) {
                    if (Objects.isNull(value) || Boolean.FALSE.equals(value)) {
                        continue;
                    }
                } else if (Objects.isNull(value) || Objects.equals("", value)) {
                    continue;
                }
                // 模糊多字段；IS_NULL/NOT_NULL 不使用字段值
                if (!nullCondition && !q.blurry().isBlank()) {
                    var blurrys = q.blurry().split(",");
                    var orPredicate = new ArrayList<Predicate>();
                    for (String s : blurrys) {
                        var name = s.trim();
                        if (name.isEmpty()) {
                            continue;
                        }
                        orPredicate.add(cb.like(root.get(name).as(String.class), "%" + value + "%"));
                    }
                    if (!orPredicate.isEmpty()) {
                        var p = new Predicate[orPredicate.size()];
                        list.add(cb.or(orPredicate.toArray(p)));
                    }
                    continue;
                }
                // 解析查询类型
                analyzeQueryType(root, query, cb, list, meta, joins);
            } catch (UtilsException e) {
                throw e;
            } catch (Exception e) {
                throw new UtilsException("解析查询字段失败: " + query.getClass().getName() + "#"
                        + meta.field().getName(), e);
            }
        }
    }

    /**
     * 解析 joinType。joins 只在单次查询内复用，避免多个字段重复创建同一路径的 join。
     *
     * @param root  /
     * @param q     /
     * @param joins 单次查询的 join 缓存
     * @return jakarta.persistence.criteria.Join
     * @date 2021/6/24 10:52 上午
     */
    @Nullable
    private static <R> Join<R, ?> analyzeJoinType(Root<R> root, Query q, Map<String, Join<R, ?>> joins) {
        var joinName = q.joinName();
        if (joinName.isBlank())
            return null;

        Join<R, ?> join = null;
        var path = "";
        for (var entity : joinName.split(">")) {
            path = path.isEmpty() ? entity : path + ">" + entity;
            var cached = joins.get(path);
            if (cached != null) {
                join = cached;
                continue;
            }
            // 第一层优先复用 root 上已有的 join
            if (join == null) {
                for (var rJoin : root.getJoins()) {
                    if (Objects.equals(rJoin.getAttribute().getName(), entity)) {
                        join = rJoin;
                        break;
                    }
                }
            }
            if (join == null) {
                join = root.join(entity, toJoinType(q.join()));
            } else if (!path.equals(entity)) {
                // 只有多级路径（roles>menus）才继续向下 join，避免把单级路径重复 join 到自己
                join = join.join(entity, toJoinType(q.join()));
            }
            joins.put(path, join);
        }
        return join;
    }

    private static JoinType toJoinType(Query.Join join) {
        return switch (join) {
            case LEFT -> JoinType.LEFT;
            case RIGHT -> JoinType.RIGHT;
            case INNER -> JoinType.INNER;
        };
    }

    /**
     * 解析query.type()。抽取主要为了方便调用
     *
     * @param root  /
     * @param query /
     * @param cb    /
     * @param list  /
     * @param field /
     * @date 2021/6/24 10:52 上午
     */
    // ? extends E:接收E类型或者E的子类型。
    // ? super E:接收E类型或者E的父类型 https://www.lwohvye.com/2021/12/04/t%e3%80%81-super-t%e3%80%81-extends-t/
    private static <R, Q> void analyzeQueryType(Root<R> root,
                                                Q query,
                                                CriteriaBuilder cb,
                                                ArrayList<Predicate> list,
                                                QueryField meta,
                                                Map<String, Join<R, ?>> joins) throws IllegalAccessException {
        var field = meta.field();
        var q = meta.query();
        var attributeName = defineAttrName(field, q);
        var value = field.get(query);
        var fieldType = boxed(field.getType());
        var comparableFieldType = value.castComparableFieldType();
        var join = analyzeJoinType(root, q, joins);

        // switch 的 -> 语法也只是语法糖
        switch (q.type()) {
            case EQUAL -> list.add(cb.equal(getExpression(attributeName, join, root).as(fieldType), value));
            case NOT_EQUAL -> list.add(cb.notEqual(getExpression(attributeName, join, root), value));
            case GREATER_THAN ->
                // var comparableFieldType = (Class<? extends Comparable>) fieldType; // 最终试下来，这一步的强转是少不了的了。
                // 需要的参数是这个样子的 (Expression<? extends Y> var1, Y var2)
                //pt1：list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(fieldType), ele));  fieldType未声明为Comparable的子类，不得行
                //pt2：list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(Comparable.class), ele));  Comparable无法转为Hibernate type，不得行
                //pt3：list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(comparableFieldType), comparableFieldType.cast(ele))); 当不采用C的方式定义时，这样也是不得行的
                    list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(comparableFieldType),
                            value.castToComparable(attributeName, comparableFieldType)));
            case LESS_THAN ->
                    list.add(cb.lessThanOrEqualTo(getExpression(attributeName, join, root).as(comparableFieldType),
                            value.castToComparable(attributeName, comparableFieldType)));
            case LESS_THAN_NQ -> list.add(cb.lessThan(getExpression(attributeName, join, root).as(comparableFieldType),
                    value.castToComparable(attributeName, comparableFieldType)));
            case INNER_LIKE ->
                    list.add(cb.like(getExpression(attributeName, join, root).as(String.class), "%" + value + "%"));
            case LEFT_LIKE -> list.add(cb.like(getExpression(attributeName, join, root).as(String.class), "%" + value));
            case RIGHT_LIKE ->
                    list.add(cb.like(getExpression(attributeName, join, root).as(String.class), value + "%"));
            case LIKE_STR ->
                    list.add(cb.like(getExpression(attributeName, join, root).as(String.class), value.toString()));
            case IN_INNER_LIKE -> {
                if (value instanceof List<?> objList && !objList.isEmpty()) {
//                                构建数组
                    var predicates = new Predicate[objList.size()];
                    for (int i = 0; i < objList.size(); i++) {
                        var obj = objList.get(i);
                        predicates[i] = cb.like(getExpression(attributeName, join, root).as(String.class), "%" + obj.toString() + "%");
                    }
//                                设置or查询
                    list.add(cb.or(predicates));
                }
            }
            case IN -> {
                if (value instanceof Collection<?> col && !col.isEmpty()) {
                    // 这里不能用fieldType.cast(value)。因为in()方法的重载，会走进in(Object... var1)中，正常要进in(Collection<?> var1)
                    list.add(getExpression(attributeName, join, root).in(col));
                }
            }
            case NOT_IN -> {
                if (value instanceof Collection<?> col && !col.isEmpty()) {
                    list.add(cb.not(getExpression(attributeName, join, root).in(col)));
                }
            }
            case BETWEEN -> {
                if (value instanceof List<?> col && !col.isEmpty()) {
                    if (col.size() != 2) {
                        throw new UtilsException("BETWEEN 需要两个值: " + attributeName);
                    }
                    var startValue = col.get(0);
                    var endValue = col.get(1);
                    if (!(startValue instanceof Comparable<?> start) || !(endValue instanceof Comparable<?> end)) {
                        throw new UtilsException("BETWEEN 的值必须实现 Comparable: " + attributeName);
                    }
                    var eleType = start.castComparableFieldType();
                    if (eleType == null) {
                        throw new UtilsException("BETWEEN 字段不是 Comparable: " + attributeName);
                    }
                    list.add(cb.between(getExpression(attributeName, join, root).as(eleType),
                            start.castToComparable(attributeName, eleType),
                            end.castToComparable(attributeName, eleType)));
                }
            }
            case NOT_NULL -> list.add(cb.isNotNull(getExpression(attributeName, join, root)));
            case IS_NULL -> list.add(cb.isNull(getExpression(attributeName, join, root)));
            case IN_OR_ISNULL -> {
                if (value instanceof Collection<?> col && !col.isEmpty()) {
                    list.add(
//                                        在集合中
                            cb.or(getExpression(attributeName, join, root).in(col)
//                                                或值为null
                                    , cb.isNull(getExpression(attributeName, join, root))
//                                                或值为空字符串
                                    , cb.equal(getExpression(attributeName, join, root).as(String.class), ""))
                    );
                }
            }
            case IS_OR_NULL -> {
                var expression = getExpression(attributeName, join, root);
                var isNull = value instanceof Number number && number.longValue() == -1L;
                list.add(isNull
                        ? cb.isNull(expression.as(fieldType))
                        : cb.equal(expression.as(fieldType), value));
            }
            case EQUAL_IN_MULTI -> {
                var predicates = new Predicate[4];
//                            like value
                predicates[0] = cb.like(getExpression(attributeName, join, root).as(String.class), value.toString());
//                            like value,%
                predicates[1] = cb.like(getExpression(attributeName, join, root).as(String.class), value + ",%");
//                            like %,value,%
                predicates[2] = cb.like(getExpression(attributeName, join, root).as(String.class), "%," + value + ",%");
//                            like %,value
                predicates[3] = cb.like(getExpression(attributeName, join, root).as(String.class), "%," + value);
//                            设置查询
                list.add(cb.or(predicates));
            }
            case FUNCTION_FIND_IN_SET ->
                // https://github.com/elunez/eladmin/pull/745
                // if we have a table with column tags， the column value of tags is comma split string, like："a,b,c"
                // we want to quey it by 'querytag' (ex 'a'), using follow sql：
                // SELECT * FROM table WHERE FIND_IN_SET('a', table.tags); 这是很常见的用法
                // FIND_IN_SET函数是IN函数的升级版.功能类似.区别在于:如果是常量，则可以直接用IN， 否则要用FIND_IN_SET()函数
                // MySQL中原型为：FIND_IN_SET(str,strlist)。 假如字符串str 在由N 子链组成的字符串列表strlist 中(子链指的是`,`分隔的字符串)，则返回值的范围在 1 到 N 之间.
                // 如果str不在strlist 或strlist 为空字符串，则返回值为 0 。如任意一个参数为NULL，则返回值为 NULL。这个函数在第一个参数包含一个逗号( , )时将无法正常运行。
                // 注意；以下只是纯sql查询的情况，这里用了cb是没问题的，in也正常，估价如果用Mybatis可能会遇到这种坑
                // select * from table where xxx in (list); // list = List.of("abc","def","str"); 这里是查不出来的，虽然list中有str，但list是变量
                // select * from table where xxx in ('abc','def','str'); // 这个可以，因为in 里面是常量
                // select * from table where FIND_IN_SET('str', list); // 这种也可以
                // 需注意，调用function后会产生结果，在外层要指定对结果的使用
                    list.add(cb.greaterThan(cb.function("FIND_IN_SET", Integer.class, cb.literal(value.toString()), getExpression(attributeName, join, root)), 0));
//            case FUNCTION_FROM_BASE64:
            // where (from_base64(user0_.description) like to_base64(user0_.description))。如何设置to_base64的参数为 fieldValue，是接下来的事情
            //    list.add(cb.like(cb.function("from_base64", fieldType, getExpression(attributeName, join, root)), cb.function("to_base64", fieldType, getExpression(attributeName, join, root)))); // 多个是支持的
            // where (from_base64(user0_.description) like '%ABC%') 。已基本可以使用
//                list.add(cb.like(cb.function("from_base64", fieldType, getExpression(attributeName, join, root)).as(String.class), "%" + value.toString() + "%")); // 这种把调用函数硬编码了
//                break; 后续移除
            case FUNCTION_4_EQUAL -> {
                if (q.functionName().isBlank()) {
                    throw new UtilsException("FUNCTION_4_EQUAL 必须配置 functionName: " + attributeName);
                }
                list.add(cb.equal(cb.function(q.functionName(), fieldType, getExpression(attributeName, join, root)), value));
            }
            default -> {
            }
        }
    }

    private static <R> Expression<?> getExpression(String attributeName, Join<R, ?> join, Root<R> root) {
        // 处理的维度是field维度的，每个field初始化一个join，若join有值，证明该field是join的实体中的，所以要从join中取，即join.get()。
        if (Objects.nonNull(join)) {
            return join.get(attributeName);
        } else {
            // 非join的，从root中取，root.get()。
            return root.get(attributeName);
        }
    }

    @NotNull
    private static String defineAttrName(Field field, Query q) {
        var propName = q.propName();
        return !propName.isBlank() ? propName : field.getName();
    }

    private static Class<?> boxed(Class<?> type) {
        if (!type.isPrimitive()) {
            return type;
        }
        if (type == long.class) return Long.class;
        if (type == int.class) return Integer.class;
        if (type == short.class) return Short.class;
        if (type == byte.class) return Byte.class;
        if (type == double.class) return Double.class;
        if (type == float.class) return Float.class;
        if (type == boolean.class) return Boolean.class;
        if (type == char.class) return Character.class;
        return type;
    }

    // 2021/11/6 使用JPA 2.1 引入的 CriteriaUpdate 和 CriteriaDelete 进行批量更新/删除。不是很实用，期待后续的使用场景
    public static <R, Q> void criteria4Update(Root<R> root, Q query, CriteriaBuilder cb) {
//        var em = new EntityManager(); //获取em
        var criteriaUpdate = cb.createCriteriaUpdate(root.getJavaType());
        criteriaUpdate.set("fieldName", "newFieldValue");
        criteriaUpdate.where(getPredicate(root, query, cb));
//        em.createQuery(criteriaUpdate).executeUpdate(); // 执行
    }

    public static <R, Q> void criteriaDelete(Root<R> root, Q query, CriteriaBuilder cb) {
//        var em = new EntityManager(); // 获取em
        var criteriaDelete = cb.createCriteriaDelete(root.getJavaType());
        criteriaDelete.where(getPredicate(root, query, cb));
//        em.createQuery(criteriaDelete).executeUpdate(); // 执行
    }
}
