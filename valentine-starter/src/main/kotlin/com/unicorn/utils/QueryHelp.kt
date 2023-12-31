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

package com.unicorn.utils

import com.unicorn.annotation.Query
import jakarta.persistence.criteria.*
import org.apache.logging.log4j.LogManager
import java.util.*
import kotlin.reflect.KProperty1
import kotlin.reflect.full.memberProperties
import kotlin.reflect.jvm.isAccessible
import kotlin.reflect.jvm.javaField

/**
 * Utility class for parsing query annotations and generating predicates for JPA criteria queries.
 */
object QueryHelp {

    private val log = LogManager.getLogger()

    /**
     * Parse the query annotation on the property and respond with the corresponding query.
     * Currently, supports simple multi-condition joint table queries,
     * but cannot support complex And, Or combined queries.
     * For these, either use QueryDSL or directly use Native SQL.
     * Although complex queries can also be supported through the integration of Annotation and Reflect,
     * personal opinion is that there is no need to reinvent the wheel, especially since JPA is compatible with it.
     * However, the reality is not very optimistic.
     *
     * @param root Root object corresponding to the table after from
     * @param query Q External criteria object
     * @param cb CriteriaBuilder factory class, used to create the criteriaQuery object for the query
     * Predicate joining of query conditions corresponds to the addition of expressions after where
     * @return jakarta.persistence.criteria.Predicate
     * @date March 31, 2021 11:57
     */
    @JvmStatic // If you use this annotation, the compiler will generate both a static method in the enclosing class of the object and an instance method in the object itself.
    fun getPredicate(root: Root<*>, query: Any?, cb: CriteriaBuilder): Predicate {
        val list = ArrayList<Predicate>()

        if (query == null) {
            log.info("Query is null! Skipped.")
        } else {
            analyzeFieldQuery(root, query, cb, list)
        }

        return cb.and(*list.toTypedArray())
    }

    @Throws(IllegalAccessException::class)
    private fun analyzeFieldQuery(root: Root<*>, query: Any, cb: CriteriaBuilder, list: ArrayList<Predicate>) {
        for (kProperty1 in query::class.memberProperties) {
            kProperty1.accessAndAnalyze(root, query, cb, list)
        }
    }

    @Throws(IllegalAccessException::class)
    private fun KProperty1<out Any, *>.accessAndAnalyze(
        root: Root<*>,
        query: Any,
        cb: CriteriaBuilder,
        list: ArrayList<Predicate>
    ) {
        val accessible = this.isAccessible
        this.isAccessible = true

        this.javaField?.let { javaField ->
            val q = javaField.getAnnotation(Query::class.java)
            val value = javaField.get(query)
            if (q != null && value.hasValue()) {
                analyzeQuery(root, cb, list, this, q, value!!)
            }
        } ?: run {
            log.error("JavaField of KProperty1<out Any, *> object is null!")
        }

        this.isAccessible = accessible
    }

    @Throws(IllegalAccessException::class)
    private fun analyzeQuery(
        root: Root<*>, cb: CriteriaBuilder, list: ArrayList<Predicate>, kProperty1: KProperty1<out Any, *>,
        q: Query, value: Any
    ) {
        if (q.blurry.isNotBlank()) {
            proceedBlurry(root, cb, list, q.blurry, value)
        } else {
            defineAttrNameAndProceedFurther(root, cb, list, kProperty1, q, value)
        }
    }

    private fun proceedBlurry(
        root: Root<*>,
        cb: CriteriaBuilder,
        list: ArrayList<Predicate>,
        blurry: String,
        value: Any
    ) {
        val blurryFields = blurry.split(",").filter { it.isNotBlank() }
        val orPredicate = blurryFields.map { cb.like(root.get<Any>(it).`as`(String::class.java), "%$value%") }
        list.add(cb.or(*orPredicate.toTypedArray()))
    }

    @Throws(IllegalAccessException::class)
    private fun defineAttrNameAndProceedFurther(
        root: Root<*>, cb: CriteriaBuilder, list: ArrayList<Predicate>, kProperty1: KProperty1<out Any, *>,
        q: Query, value: Any
    ) {
        val attributeName = q.propName.takeIf { it.isNotBlank() } ?: kProperty1.name
        val join = analyzeJoinType(root, q, value)
        proceedQueryType(root, cb, list, kProperty1, q, attributeName, join, value)
    }

    private fun analyzeJoinType(root: Root<*>, q: Query?, value: Any?): Join<out Any, *>? {
        var join: Join<out Any, *>? = null
        q?.joinName?.trim()?.split(">".toRegex())?.filter { it.isNotEmpty() }?.forEach { entity ->
            if (join == null) {
                root.joins.firstOrNull { rJoin -> Objects.equals(rJoin.attribute.name, entity) }?.let {
                    join = it
                    return@forEach
                }
            }

            join = when (q.join) {
                Query.Join.LEFT -> root.createJoinEntity(entity, JoinType.LEFT, value, join)
                Query.Join.RIGHT -> root.createJoinEntity(entity, JoinType.RIGHT, value, join)
                Query.Join.INNER -> root.createJoinEntity(entity, JoinType.INNER, value, join)
            }
        }
        return join
    }

    private fun <X, Z : Any> Root<Z>.createJoinEntity(
        entity: String,
        joinType: JoinType,
        value: Any?,
        join: Join<out Any, *>?
    ): Join<X, String>? {
        return when (value != null && join != null) {
            true -> join.join(entity, joinType)
            false -> this.join(entity, joinType)
        }
    }

    @Throws(IllegalAccessException::class)
    private fun proceedQueryType(
        root: Root<*>, cb: CriteriaBuilder, list: ArrayList<Predicate>, kProperty1: KProperty1<out Any, *>, q: Query,
        attributeName: String, join: Join<out Any, *>?, value: Any
    ) {
        val fieldType = kProperty1.javaField?.type
        val comparableFieldType = value.castToComparableFieldType()
        val baseExpression = getExpression(attributeName, join, root)
        when (q.type) {
            Query.Type.EQUAL -> list.add(cb.equal(baseExpression.`as`(fieldType), value))
            Query.Type.NOT_EQUAL -> list.add(cb.notEqual(baseExpression, value))

            Query.Type.GREATER_THAN -> list.add(
                cb.greaterThanOrEqualTo(
                    baseExpression.`as`(comparableFieldType),
                    value.castToComparable(comparableFieldType)
                )
            )

            Query.Type.LESS_THAN -> list.add(
                cb.lessThanOrEqualTo(
                    baseExpression.`as`(comparableFieldType),
                    value.castToComparable(comparableFieldType)
                )
            )

            Query.Type.LESS_THAN_NQ -> list.add(
                cb.lessThan(
                    baseExpression.`as`(comparableFieldType),
                    value.castToComparable(comparableFieldType)
                )
            )

            Query.Type.INNER_LIKE -> list.add(createStringLikePredicate(cb, baseExpression, value))
            Query.Type.LEFT_LIKE -> list.add(createStringLikePredicate(cb, baseExpression, value, true, false))
            Query.Type.RIGHT_LIKE -> list.add(createStringLikePredicate(cb, baseExpression, value, false, true))
            Query.Type.LIKE_STR -> list.add(cb.like(baseExpression.`as`(String::class.java), value.toString()))
            Query.Type.IN_INNER_LIKE -> {
                if (value is List<*>) {
                    // 构建数组
                    val predicates = arrayOfNulls<Predicate>(value.size)
                    for (i in value.indices) {
                        val obj: Any = value[i]!!
                        predicates[i] = createStringLikePredicate(cb, baseExpression, obj)
                    }
                    // 设置or查询
                    list.add(cb.or(*predicates))
                }
            }

            Query.Type.IN -> {
                if (value is Collection<*> && value.isNotEmpty()) {
                    // 这里不能用fieldType.cast(val)。因为in()方法的重载，会走进in(Object... var1)中，正常要进in(Collection<?> var1)
                    list.add(baseExpression.`in`(value))
                }
            }

            Query.Type.NOT_IN -> {
                if (value is Collection<*> && value.isNotEmpty()) {
                    list.add(cb.not(baseExpression.`in`(value)))
                }
            }

            Query.Type.BETWEEN -> {
                if (value is List<*> && value.size == 2 && value[0] is Comparable<*> && value[1] is Comparable<*>) {
                    val start = value[0] as Comparable<*>
                    val end = value[1] as Comparable<*>
                    val eleType = start.castToComparableFieldType()
                    list.add(
                        cb.between(
                            baseExpression.`as`(eleType),
                            start.castToComparable(eleType),
                            eleType!!.cast(end)
                        )
                    )
                }
            }

            Query.Type.NOT_NULL -> list.add(cb.isNotNull(baseExpression))
            Query.Type.IS_NULL -> list.add(cb.isNull(baseExpression))

            Query.Type.EQUAL_IN_MULTI -> {
                val predicates = arrayOfNulls<Predicate>(4)
                // like val
                predicates[0] = createStringLikePredicate(cb, baseExpression, "$value", false, false)
                // like val,%
                predicates[1] = createStringLikePredicate(cb, baseExpression, "$value,", false, true)
                // like %,val,%
                predicates[2] = createStringLikePredicate(cb, baseExpression, ",$value,")
                // like %,val
                predicates[3] = createStringLikePredicate(cb, baseExpression, ",$value", true, false)
                // 设置查询
                list.add(cb.or(*predicates))
            }

            Query.Type.FUNCTION_FIND_IN_SET ->
                list.add(
                    cb.greaterThan(
                        cb.function(
                            "FIND_IN_SET",
                            Int::class.java,
                            cb.literal(value.toString()),
                            baseExpression
                        ), 0
                    )
                )

            Query.Type.FUNCTION_4_EQUAL -> list.add(
                cb.equal(
                    cb.function(
                        q.functionName,
                        fieldType,
                        baseExpression
                    ), value
                )
            )
            //... other Query.Type values will have similar methods
        }
    }

    private fun Any?.hasValue() = this != null && (this !is String || this.trim() != "")

    private fun getExpression(attributeName: String, join: Join<out Any, *>?, root: Root<*>) =
        join?.get<Any>(attributeName) ?: root[attributeName]

    private fun createStringLikePredicate(
        cb: CriteriaBuilder,
        baseExpression: Expression<*>,
        value: Any,
        prefix: Boolean = true,
        postfix: Boolean = true
    ): Predicate {
        val format = "${if (prefix) "%" else ""}$value${if (postfix) "%" else ""}"
        return cb.like(baseExpression.`as`(String::class.java), format)
    }

    // Extension Functions
    private fun Any.castToComparableFieldType(): Class<out Comparable<Any>>? = if (this is Comparable<*>)
        @Suppress("UNCHECKED_CAST")
        this::class.java as Class<out Comparable<Any?>?> else null

    private fun Any.castToComparable(fieldType: Class<out Comparable<Any>>?)
            : Comparable<Any> = Objects.requireNonNull(fieldType)!!.cast(this)

    private fun String?.isBlank(): Boolean = this == null || this.trim() == ""

}
