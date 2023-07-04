package com.unicorn.utils

import com.lwohvye.core.utils.ExceptionMsgUtils
import com.lwohvye.core.utils.StringUtils
import com.unicorn.annotation.Query
import jakarta.persistence.criteria.*
import org.apache.logging.log4j.LogManager
import java.util.*
import kotlin.reflect.KProperty1
import kotlin.reflect.full.memberProperties
import kotlin.reflect.jvm.isAccessible
import kotlin.reflect.jvm.javaField

object QueryHelp {

    private val log = LogManager.getLogger()

    /**
     * 解析属性上的查询注解。贫瘠相应的查询
     * 当前已经支持了简单多条件的连表查询，但无法支持复杂的 And, Or 组合查询，这种要么使用QueryDSL要么干脆使用Native SQL。
     * 虽说通过整合Annotation和Reflect 也是可以支持复杂查询的，但既然已经有QueryDSL（虽然用的不多且好久没更新了），个人认为没必要重复造轮子，尤其是JPA跟其还是兼容的
     * 但现实情况不太乐观
     *
     * @param root  Root根对象对应于from后面的表
     * @param query Q 外部的criteria对象
     * @param cb    CriteriaBuilder工厂类，用于创建查询的criteriaQuery对象
     * Predicate查询条件的拼接对应于where后面的添加表达式
     * @return jakarta.persistence.criteria.Predicate
     * @date 2021/3/31 11:57
     */
    @JvmStatic // If you use this annotation, the compiler will generate both a static method in the enclosing class of the object and an instance method in the object itself.
    fun getPredicate(root: Root<*>, query: Any?, cb: CriteriaBuilder): Predicate {
        val list = ArrayList<Predicate>()
        if (query == null) {
            log.info("query is null! skipped")
            return cb.and(*list.toTypedArray<Predicate>())
        }
        // 根据Field及上面的Annotation，拼接Query & Join
        analyzeFieldQuery(root, query, cb, list)
        // 各Field的Condition通过And连接，这是比较常见的场景
        return cb.and(*list.toTypedArray<Predicate>())
    }

    @Throws(IllegalAccessException::class)
    private fun analyzeFieldQuery(root: Root<*>, query: Any, cb: CriteriaBuilder, list: ArrayList<Predicate>) {
        for (kProperty1 in query::class.memberProperties) {
            val accessible = kProperty1.isAccessible
            // boolean accessible = field.isAccessible(); // 方法已过期，改用canAccess
            // 设置对象的访问权限，保证对private的属性的访
            kProperty1.isAccessible = true
            val q = kProperty1.javaField?.getAnnotation(Query::class.java) // annotation需要先转成Java才能拿到，Kotlin拿不到。。。
            if (q != null) {
                val blurry = q.blurry
                val value = kProperty1.javaField?.get(query)
                if (value == null || value == "") {
                    continue
                }
                // 模糊多字段
                if (StringUtils.isNotBlank(blurry)) {
                    val blurrys = blurry.split(",".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
                    val orPredicate = ArrayList<Predicate>()
                    for (s in blurrys) {
                        orPredicate.add(cb.like(root.get<Any>(s).`as`(String::class.java), "%$value%"))
                    }
                    val p = arrayOfNulls<Predicate>(orPredicate.size)
                    list.add(cb.or(*orPredicate.toArray(p)))
                    continue
                }
                // 解析查询类型
                analyzeQueryType(root, query, cb, list, kProperty1)
            }
            kProperty1.isAccessible = accessible // 该回原来的属性
        }
    }

    /**
     * 解析joinType
     *
     * @param root /
     * @param q    /
     * @param value  /
     * @return jakarta.persistence.criteria.Join
     * @date 2021/6/24 10:52 上午
     */
    private fun analyzeJoinType(root: Root<*>, q: Query?, value: Any?): Join<out Any, *>? {
        var join: Join<out Any, *>? = null
        val joinName = q?.joinName
        if (joinName == null || joinName.trim() == "") return null
        // 首先获取已经设置的join。只用一次的话，使用聚合会降低性能，所以再次调整为循环的方式
        // var existJoinNames = root.getJoins().stream().collect(Collectors.toMap(rJoin -> rJoin.getAttribute().getName(), rJoin -> rJoin, (o, o2) -> o2)); 只用一次，聚合不划算
        // 这里支持属性套属性。比如查User时，配置了连Role表 joinName = "roles"，若需要用Role中的Menus属性做一些过滤，则 joinName = "roles>menus" 这样配置即可，此时会连上sys_roles_menus和sys_menu两张表
        val joinNames = joinName.split(">".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
        checkJoin@ for (entity in joinNames) {
            // 若join已经有值了，就不走下面这段逻辑了。这里还保证了如果使用了>，只有第一层会走进来，避免一些问题，比如 roles>dept 和 dept。这俩个dept是不应用同一个join的
            // 业务中应该没有需要对同一张table多次join，甚至joinType还不同的情形。这里是不支持此类场景的
            if (join == null) {
                for (rJoin in root.joins) {
                    // 若已经设置过该joinName，则将已设置的rJoin赋值给join，开启下一循环
                    if (Objects.equals(rJoin.attribute.name, entity)) {
                        join = rJoin
                        continue@checkJoin
                    }
                }
            }
            // Switch Expressions。这个只是一个sweets 语法糖
            val stubJoin = join != null && value != null
            join = when (q.join) {
                Query.Join.LEFT -> if (stubJoin) join!!.join(entity, JoinType.LEFT) else root.join<Any, Any>(
                    entity,
                    JoinType.LEFT
                )

                Query.Join.RIGHT -> if (stubJoin) join!!.join(entity, JoinType.RIGHT) else root.join<Any, Any>(
                    entity,
                    JoinType.RIGHT
                )

                Query.Join.INNER -> if (stubJoin) join!!.join(entity, JoinType.INNER) else root.join<Any, Any>(
                    entity,
                    JoinType.INNER
                )
            }
        }
        return join
    }

    /**
     * 解析query.type()。抽取主要为了方便调用
     *
     * @param root  /
     * @param query /
     * @param cb    /
     * @param list  /
     * @param kProperty1 /
     * @date 2021/6/24 10:52 上午
     */
    // ? extends E:接收E类型或者E的子类型。
    // ? super E:接收E类型或者E的父类型 https://www.lwohvye.com/2021/12/04/t%e3%80%81-super-t%e3%80%81-extends-t/
    @Throws(IllegalAccessException::class)
    private fun analyzeQueryType(
        root: Root<*>,
        query: Any,
        cb: CriteriaBuilder,
        list: ArrayList<Predicate>,
        kProperty1: KProperty1<out Any, *>
    ) {
        val q = kProperty1.javaField?.getAnnotation(Query::class.java)
        val attributeName = defineAttrName(kProperty1, q)
        // 经过上一级方法，这里的value一定是NoNull的
        val value = kProperty1.javaField!!.get(query)
        val fieldType = kProperty1.javaField?.type // type需要JavaType，kProperty1.javaClass是KotlinType
        val comparableFieldType = value.castComparableFieldType()
        val join = analyzeJoinType(root, q, value)
        when (q!!.type) {
            Query.Type.EQUAL -> list.add(cb.equal(getExpression(attributeName, join, root).`as`(fieldType), value))
            Query.Type.NOT_EQUAL -> list.add(cb.notEqual(getExpression(attributeName, join, root), value))
            Query.Type.GREATER_THAN ->
                list.add(
                    cb.greaterThanOrEqualTo(
                        getExpression(attributeName, join, root).`as`(comparableFieldType),
                        value.castToComparable(attributeName, comparableFieldType)
                    )
                )

            Query.Type.LESS_THAN -> list.add(
                cb.lessThanOrEqualTo(
                    getExpression(attributeName, join, root).`as`(comparableFieldType),
                    value.castToComparable(attributeName, comparableFieldType)
                )
            )

            Query.Type.LESS_THAN_NQ -> list.add(
                cb.lessThan(
                    getExpression(attributeName, join, root).`as`(comparableFieldType),
                    value.castToComparable(attributeName, comparableFieldType)
                )
            )

            Query.Type.INNER_LIKE -> list.add(
                cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), "%$value%"
                )
            )

            Query.Type.LEFT_LIKE -> list.add(
                cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), "%$value"
                )
            )

            Query.Type.RIGHT_LIKE -> list.add(
                cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), "$value%"
                )
            )

            Query.Type.LIKE_STR -> list.add(
                cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), value.toString()
                )
            )

            Query.Type.IN_INNER_LIKE -> {
                if (value is List<*>) {
                    // 构建数组
                    val predicates = arrayOfNulls<Predicate>(value.size)
                    for (i in value.indices) {
                        val obj: Any = value[i]!!
                        predicates[i] = cb.like(
                            getExpression(attributeName, join, root).`as`(
                                String::class.java
                            ), "%$obj%"
                        )
                    }
                    // 设置or查询
                    list.add(cb.or(*predicates))
                }
            }

            Query.Type.IN -> {
                if (value is Collection<*> && value.isNotEmpty()) {
                    // 这里不能用fieldType.cast(val)。因为in()方法的重载，会走进in(Object... var1)中，正常要进in(Collection<?> var1)
                    list.add(getExpression(attributeName, join, root).`in`(value))
                }
            }

            Query.Type.NOT_IN -> {
                if (value is Collection<*> && value.isNotEmpty()) {
                    list.add(cb.not(getExpression(attributeName, join, root).`in`(value)))
                }
            }

            Query.Type.BETWEEN -> {
                if (value is List<*> && value.size == 2 && value[0] is Comparable<*> && value[1] is Comparable<*>) {
                    val start = value[0] as Comparable<*>
                    val end = value[1] as Comparable<*>
                    val eleType = start.castComparableFieldType()
                    list.add(
                        cb.between(
                            getExpression(attributeName, join, root).`as`(eleType),
                            start.castToComparable(attributeName, eleType),
                            eleType!!.cast(end)
                        )
                    )
                }
            }

            Query.Type.NOT_NULL -> list.add(cb.isNotNull(getExpression(attributeName, join, root)))
            Query.Type.IS_NULL -> list.add(cb.isNull(getExpression(attributeName, join, root)))
            Query.Type.IN_OR_ISNULL -> {
                if (value is Collection<*> && value.isNotEmpty()) {
                    list.add( // 在集合中
                        cb.or(
                            getExpression(
                                attributeName,
                                join,
                                root
                            ).`in`(value) // 或值为null
                            ,
                            cb.isNull(
                                getExpression(attributeName, join, root)
                            ) // 或值为空字符串
                            ,
                            cb.equal(
                                getExpression(attributeName, join, root).`as`(
                                    String::class.java
                                ), ""
                            )
                        )
                    )
                }
            }

            Query.Type.IS_OR_NULL -> list.add(
                if (value as Long == -1L) cb.isNull(
                    getExpression(attributeName, join, root).`as`(fieldType)
                ) else cb.equal(
                    getExpression(attributeName, join, root).`as`(fieldType), value
                )
            )

            Query.Type.EQUAL_IN_MULTI -> {
                val predicates = arrayOfNulls<Predicate>(4)
                // like val
                predicates[0] = cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), value.toString()
                )
                // like val,%
                predicates[1] = cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), "$value,%"
                )
                // like %,val,%
                predicates[2] = cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), "%,$value,%"
                )
                // like %,val
                predicates[3] = cb.like(
                    getExpression(attributeName, join, root).`as`(
                        String::class.java
                    ), "%,$value"
                )
                // 设置查询
                list.add(cb.or(*predicates))
            }

            Query.Type.FUNCTION_FIND_IN_SET ->  // https://github.com/elunez/eladmin/pull/745
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
                list.add(
                    cb.greaterThan(
                        cb.function(
                            "FIND_IN_SET",
                            Int::class.java,
                            cb.literal(value.toString()),
                            getExpression(attributeName, join, root)
                        ), 0
                    )
                )

            Query.Type.FUNCTION_4_EQUAL -> list.add(
                cb.equal(
                    cb.function(
                        q.functionName,
                        fieldType,
                        getExpression(attributeName, join, root)
                    ), value
                )
            )

        }
    }

    private fun getExpression(attributeName: String, join: Join<out Any, *>?, root: Root<*>): Expression<*> =
        // 处理的维度是field维度的，每个field初始化一个join，若join有值，证明该field是join的实体中的，所以要从join中取，即join.get()。
        join?.get<Any>(attributeName) ?:
        // 非join的，从root中取，root.get()。
        root.get<Any>(attributeName)


    private fun defineAttrName(kProperty1: KProperty1<out Any, *>, q: Query?): String {
        val propName = q?.propName
        return if (propName != null && propName.trim() != "") propName else kProperty1.name
    }

    /**
     * 构建一个Comparable Type 的 fieldType，跟fieldType一样或者为null（这里返回null是因为如果不是Comparable，那些比较类的Query是无法invoke的）
     * https://www.lwohvye.com/2021/12/04/t%e3%80%81-super-t%e3%80%81-extends-t/
     *
     * @return java.lang.Class
     * @date 2023/06/22 5:21 PM
     */
    // Extension Functions
    private fun Any.castComparableFieldType(): Class<out Comparable<Any>>? = if (this is Comparable<*>)
        @Suppress("UNCHECKED_CAST")
        this::class.java as Class<out Comparable<Any?>?> else null


    // Extension Functions
    private fun Any.castToComparable(
        attributeName: String,
        tClass: Class<out Comparable<Any>>?
    ): Comparable<Any> =
        Objects.requireNonNull(
            tClass,
            ExceptionMsgUtils.genUnComparableExcMsg(attributeName)
        )!!.cast(this)


    private fun String?.isBlank(): Boolean = this == null || this.trim() == ""

}
