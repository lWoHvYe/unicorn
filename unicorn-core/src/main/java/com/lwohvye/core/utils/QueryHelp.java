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

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.core.annotation.DataPermission;
import com.lwohvye.core.annotation.Query;
import jakarta.persistence.criteria.*;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * @author Zheng Jie, lWoHvYe
 * @date 2019-6-4 14:59:48
 */
@Slf4j
// @SuppressWarnings 抑制警告 https://www.lwohvye.com/2021/12/05/suppresswarnings%e6%b3%a8%e8%a7%a3%e7%94%a8%e6%b3%95/
// @SuppressWarnings({"unchecked", "rawtypes", "unused"})
public class QueryHelp {

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
        // 数据权限验证
        analyzeDataPermission(root, query, list);
        try {
            // 根据Field及上面的Annotation，拼接Query & Join
            analyzeFieldQuery(root, query, cb, list);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        // 各Field的Condition通过And连接，这是比较常见的场景
        return cb.and(list.toArray(new Predicate[0]));
    }

    /**
     * 数据权限验证
     *
     * @param root  /
     * @param query /
     * @param list  /
     * @date 2021/11/7 4:36 下午
     */
    private static <R, Q> void analyzeDataPermission(Root<R> root, Q query, ArrayList<Predicate> list) {
        DataPermission permission = query.getClass().getAnnotation(DataPermission.class);
        if (permission != null) {
            // 获取数据权限
            var dataScopes = SecurityUtils.getCurrentUserDataScope();
            if (CollUtil.isNotEmpty(dataScopes) && StringUtils.isNotBlank(permission.fieldName())) {
                if (StringUtils.isNotBlank(permission.joinName())) {
                    // 因为首先处理这部分，不必担心join重复。这里用var的化，join的类型会是Join<Object, Object>
                    Join<R, ?> join = root.join(permission.joinName(), JoinType.LEFT);
                    list.add(getExpression(permission.fieldName(), join, root).in(dataScopes));
                } else {
                    list.add(getExpression(permission.fieldName(), null, root).in(dataScopes));
                }
            }
        }
    }

    private static <R, Q> void analyzeFieldQuery(Root<R> root, Q query, CriteriaBuilder cb, ArrayList<Predicate> list) throws IllegalAccessException {
        for (var field : ReflectUtil.getFields(query.getClass())) {
            // AccessibleObject 类是 Field、Method 和 Constructor 对象的基类。它提供了将反射的对象标记为在使用时取消默认 Java 语言访问控制检查的能力。
            // 对于公共成员、默认（打包）访问成员、受保护成员和私有成员，在分别使用 Field、Method 或 Constructor 对象来设置或获得字段、调用方法，或者创建和初始化类的新实例的时候，会执行访问检查。
            // 在反射对象中设置 accessible 标志允许具有足够特权的复杂应用程序（比如 Java Object Serialization 或其他持久性机制）以某种通常禁止使用的方式来操作对象，
            // 需注意flag=true：指示反射的对象在使用时应该取消 Java 语言访问检查。flag=false：指示反射的对象应该实施 Java 语言访问检查。
            // 所以setAccessible只是启用和禁用访问安全检查的开关,并不是为true就能访问，为false就不能访问
            // ⚠️ Reflect获取的都是副本Duplicate，对其的Modifier理论上不会影响其他地方获取的结果，但有的框架或工具会对Reflect做Cache，所以建议在Use后Reset Accessible
            // field.canAccess(filed对应的查询器实例)
            var accessible = field.canAccess(query);
            // boolean accessible = field.isAccessible(); // 方法已过期，改用canAccess
            // 设置对象的访问权限，保证对private的属性的访
            // field.setAccessible(true); // 用下面这种方式
            if (!field.trySetAccessible()) // 设置成功/或本就是true。会返回true
                continue; // 若设置失败，则跳过
            Query q = field.getAnnotation(Query.class);
            if (q != null) {
                var blurry = q.blurry();
                var val = field.get(query);
                if (Objects.isNull(val) || Objects.equals("", val)) {
                    continue;
                }
                // 模糊多字段
                if (StringUtils.isNotBlank(blurry)) {
                    var blurrys = blurry.split(",");
                    var orPredicate = new ArrayList<Predicate>();
                    for (String s : blurrys) {
                        orPredicate.add(cb.like(root.get(s).as(String.class), "%" + val + "%"));
                    }
                    var p = new Predicate[orPredicate.size()];
                    list.add(cb.or(orPredicate.toArray(p)));
                    continue;
                }
                // 解析查询类型
                analyzeQueryType(root, query, cb, list, field);
            }
            field.setAccessible(accessible); // 该回原来的属性
        }
    }

    /**
     * 解析joinType
     *
     * @param root /
     * @param q    /
     * @param val  /
     * @return jakarta.persistence.criteria.Join
     * @date 2021/6/24 10:52 上午
     */
    @Nullable
    private static <R> Join<R, ?> analyzeJoinType(Root<R> root, Query q, Object val) {
        Join<R, ?> join = null;
        var joinName = q.joinName();
        if (StringUtils.isBlank(joinName))
            return null;
        // 首先获取已经设置的join。只用一次的话，使用聚合会降低性能，所以再次调整为循环的方式
        // var existJoinNames = root.getJoins().stream().collect(Collectors.toMap(rJoin -> rJoin.getAttribute().getName(), rJoin -> rJoin, (o, o2) -> o2)); 只用一次，聚合不划算
        // 这里支持属性套属性。比如查User时，配置了连Role表 joinName = "roles"，若需要用Role中的Menus属性做一些过滤，则 joinName = "roles>menus" 这样配置即可，此时会连上sys_roles_menus和sys_menu两张表
        var joinNames = joinName.split(">");

        for (var entity : joinNames) {
            // 若join已经有值了，就不走下面这段逻辑了。这里还保证了如果使用了>，只有第一层会走进来，避免一些问题，比如 roles>dept 和 dept。这俩个dept是不应用同一个join的
            // 业务中应该没有需要对同一张table多次join，甚至joinType还不同的情形。这里是不支持此类场景的
            checkJoin:
            {
                if (Objects.isNull(join)) {
//                    var rJoin = existJoinNames.get(entity); 同上
                    for (var rJoin : root.getJoins()) {
                        // 若已经设置过该joinName，则将已设置的rJoin赋值给join，开启下一循环
                        if (Objects.equals(rJoin.getAttribute().getName(), entity)) {
                            join = rJoin;
                            break checkJoin;
                        }
                    }
                }
                // Switch Expressions。这个只是一个sweets 语法糖
                var stubJoin = Objects.nonNull(join) && Objects.nonNull(val);
                join = switch (q.join()) {
                    case LEFT:
                        yield stubJoin ? join.join(entity, JoinType.LEFT) : root.join(entity, JoinType.LEFT);
                    case RIGHT:
                        yield stubJoin ? join.join(entity, JoinType.RIGHT) : root.join(entity, JoinType.RIGHT);
                    case INNER:
                        yield stubJoin ? join.join(entity, JoinType.INNER) : root.join(entity, JoinType.INNER);
                };
            }
        }
        return join;
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
                                                Field field) throws IllegalAccessException {
        Query q = field.getAnnotation(Query.class);
        var attributeName = defineAttrName(field, q);
        var val = field.get(query);
        var fieldType = field.getType();
        var comparableFieldType = castComparableFieldType(val);
        var join = analyzeJoinType(root, q, val);

        // switch 的 -> 语法也只是语法糖
        switch (q.type()) {
            case EQUAL -> list.add(cb.equal(getExpression(attributeName, join, root).as(fieldType), val));
            case NOT_EQUAL -> list.add(cb.notEqual(getExpression(attributeName, join, root), val));
            case GREATER_THAN ->
                // var comparableFieldType = (Class<? extends Comparable>) fieldType; // 最终试下来，这一步的强转是少不了的了。
                // 需要的参数是这个样子的 (Expression<? extends Y> var1, Y var2)
                //pt1：list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(fieldType), ele));  fieldType未声明为Comparable的子类，不得行
                //pt2：list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(Comparable.class), ele));  Comparable无法转为Hibernate type，不得行
                //pt3：list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(comparableFieldType), comparableFieldType.cast(ele))); 当不采用C的方式定义时，这样也是不得行的
                    list.add(cb.greaterThanOrEqualTo(getExpression(attributeName, join, root).as(comparableFieldType),
                            Objects.requireNonNull(comparableFieldType, ExceptionMsgUtils.genUnComparableExcMsg(attributeName)).cast(val)));
            case LESS_THAN ->
                    list.add(cb.lessThanOrEqualTo(getExpression(attributeName, join, root).as(comparableFieldType),
                            Objects.requireNonNull(comparableFieldType, ExceptionMsgUtils.genUnComparableExcMsg(attributeName)).cast(val)));
            case LESS_THAN_NQ -> list.add(cb.lessThan(getExpression(attributeName, join, root).as(comparableFieldType),
                    Objects.requireNonNull(comparableFieldType, ExceptionMsgUtils.genUnComparableExcMsg(attributeName)).cast(val)));
            case INNER_LIKE ->
                    list.add(cb.like(getExpression(attributeName, join, root).as(String.class), "%" + val + "%"));
            case LEFT_LIKE -> list.add(cb.like(getExpression(attributeName, join, root).as(String.class), "%" + val));
            case RIGHT_LIKE -> list.add(cb.like(getExpression(attributeName, join, root).as(String.class), val + "%"));
            case LIKE_STR ->
                    list.add(cb.like(getExpression(attributeName, join, root).as(String.class), val.toString()));
            case IN_INNER_LIKE -> {
                if (val instanceof List<?> objList) {
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
                if (val instanceof Collection<?> col && !col.isEmpty()) {
                    // 这里不能用fieldType.cast(val)。因为in()方法的重载，会走进in(Object... var1)中，正常要进in(Collection<?> var1)
                    list.add(getExpression(attributeName, join, root).in(col));
                }
            }
            case NOT_IN -> {
                if (val instanceof Collection<?> col && !col.isEmpty()) {
                    list.add(cb.not(getExpression(attributeName, join, root).in(col)));
                }
            }
            case BETWEEN -> {
                if (val instanceof List<?> col && col.size() == 2 && col.get(0) instanceof Comparable<?> start && col.get(1) instanceof Comparable<?> end) {
                    var eleType = castComparableFieldType(start);
                    list.add(cb.between(getExpression(attributeName, join, root).as(eleType),
                            Objects.requireNonNull(eleType, ExceptionMsgUtils.genUnComparableExcMsg(attributeName)).cast(start), eleType.cast(end)));
                }
            }
            case NOT_NULL -> list.add(cb.isNotNull(getExpression(attributeName, join, root)));
            case IS_NULL -> list.add(cb.isNull(getExpression(attributeName, join, root)));
            case IN_OR_ISNULL -> {
                if (val instanceof Collection<?> col && !col.isEmpty()) {
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
            case IS_OR_NULL -> list.add((Long) val == -1L ?
                    cb.isNull(getExpression(attributeName, join, root).as(fieldType)) :
                    cb.equal(getExpression(attributeName, join, root).as(fieldType), val));
            case EQUAL_IN_MULTI -> {
                var predicates = new Predicate[4];
//                            like val
                predicates[0] = cb.like(getExpression(attributeName, join, root).as(String.class), val.toString());
//                            like val,%
                predicates[1] = cb.like(getExpression(attributeName, join, root).as(String.class), val + ",%");
//                            like %,val,%
                predicates[2] = cb.like(getExpression(attributeName, join, root).as(String.class), "%," + val + ",%");
//                            like %,val
                predicates[3] = cb.like(getExpression(attributeName, join, root).as(String.class), "%," + val);
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
                    list.add(cb.greaterThan(cb.function("FIND_IN_SET", Integer.class, cb.literal(val.toString()), getExpression(attributeName, join, root)), 0));
//            case FUNCTION_FROM_BASE64:
            // where (from_base64(user0_.description) like to_base64(user0_.description))。如何设置to_base64的参数为 fieldValue，是接下来的事情
            //    list.add(cb.like(cb.function("from_base64", fieldType, getExpression(attributeName, join, root)), cb.function("to_base64", fieldType, getExpression(attributeName, join, root)))); // 多个是支持的
            // where (from_base64(user0_.description) like '%ABC%') 。已基本可以使用
//                list.add(cb.like(cb.function("from_base64", fieldType, getExpression(attributeName, join, root)).as(String.class), "%" + val.toString() + "%")); // 这种把调用函数硬编码了
//                break; 后续移除
            case FUNCTION_4_EQUAL ->
                    list.add(cb.equal(cb.function(q.functionName(), fieldType, getExpression(attributeName, join, root)), val));
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
        return StringUtils.isNotBlank(propName) ? propName : field.getName();
    }

    /**
     * 构建一个Comparable Type 的 fieldType，跟fieldType一样或者为null（这里返回null是因为如果不是Comparable，那些比较类的Query是无法invoke的）
     * 这个参数只是为了解决几个警告，因为fieldType不一定extends Comparable，所以加了这个，来限定需要能够比较才行。
     * 因为cb.lessThan,greaterThan,between的返回值 <Y extends Comparable<? super Y>>， 入参 （Expression<? extends Y> var1, Y var2, Y var3），含义:类型 Y 必须实现 Comparable 接口，并且这个接口的类型是 Y 或 Y 的任一父类
     * https://www.lwohvye.com/2021/12/04/t%e3%80%81-super-t%e3%80%81-extends-t/
     *
     * @param val /
     * @return java.lang.Class
     * @date 2022/9/17 5:21 PM
     */
    @Nullable
    // private static <C> Class<? extends Comparable<? super C>> castComparableFieldType(Object val) 因为C 未指定，所以实际上是Object，? super Object只能是Object，所以干脆就用Object了
    // 2022/9/17 总感觉这里是不对的，`? extends Comparable<Object>` 和 `? extends Comparable<? super Y>` 的含义是不一样的，
    //  Any Class implements Comparable<Object> 与 Any Class implements Comparable & the type of this interface is Y or Y's parent，but 这当前业务中，Y是没办法被define的
    //  明明应该用后者，但前者这种编译能通过，运行也不报错，但总感觉哪里不对，泛型还需进一步的学习(显然跟泛型擦除有关, 因为采用了类型擦除，泛型类型只在静态类型检查时期存在，在这之后，程序中所有的泛型类型都会被擦除，并替换为它们的非泛型上界)
    //  疑问是 String implements Comparable<String>, Timestamp extends Date (Date implements Comparable<Date>) 与 XXX implements Comparable<Object> 区别与相似之处
    // 2022/9/18 结合字节码，Comparable<T> 中的 compareTo方法，依旧是compareTo(Object obj)，这样上面的疑问便解决了:他们实际上是一样的，只是内部有的有转型
    /*
    // class version 62.0 (62)
    // access flags 0x601
    // signature <T:Ljava/lang/Object;>Ljava/lang/Object;
    // declaration: java/lang/Comparable<T>
    public abstract interface java/lang/Comparable {

    // compiled from: Comparable.java

    // access flags 0x401
    // signature (TT;)I
    // declaration: int compareTo(T)
    public abstract compareTo(Ljava/lang/Object;)I
    }
     */
    @SuppressWarnings("unchecked")
    private static Class<? extends Comparable<Object>> castComparableFieldType(Object val) {
        return val instanceof Comparable<?> cec ? (Class<? extends Comparable<Object>>) cec.getClass() : null;
    }

    /* 在此记录几种行不通的方案，以防后续忘记再次做重复的尝试
    private static <Y extends Comparable<? super Y>> Class<? extends Y> castComparableFieldType(Object val) {} // 基于调用的方式，build时可能报错，并且这个Y感觉没办法define
    private static <Y extends Comparable<? super Y>> Class<? extends Y> castComparableFieldType(Object val, Class<Y> yClass) {} // 当前业务中yClass是没法define的，且还有泛型上的限制，最重要的，如果yClass明了，这方法没调用的必要了
    private static <Y> Class<Y extends Comparable<? super Y>> castComparableFieldType(Object val) {} // 直接语法错误 Unexpected bound
    */

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
