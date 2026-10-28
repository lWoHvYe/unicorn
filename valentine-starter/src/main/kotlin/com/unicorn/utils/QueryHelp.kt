/*
 *    Copyright (c) 2024-2026.  lWoHvYe(Hongyan Wang)
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
import java.util.concurrent.ConcurrentHashMap
import kotlin.reflect.KClass
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
     * criteria 类型的属性元数据只解析一次。
     */
    private val propertyCache = ConcurrentHashMap<KClass<*>, List<KProperty1<out Any, *>>>()

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
            // joins 只在单次查询内复用
            analyzeFieldQuery(root, query, cb, list, HashMap())
        }

        return cb.and(*list.toTypedArray())
    }

    private fun analyzeFieldQuery(
        root: Root<*>,
        query: Any,
        cb: CriteriaBuilder,
        list: ArrayList<Predicate>,
        joins: MutableMap<String, Join<*, *>>
    ) {
        for (kProperty1 in propertyCache.computeIfAbsent(query::class) { it.memberProperties.toList() }) {
            try {
                kProperty1.accessAndAnalyze(root, query, cb, list, joins)
            } catch (e: Exception) {
                throw IllegalStateException("解析查询字段失败: ${query::class.qualifiedName}#${kProperty1.name}", e)
            }
        }
    }

    private fun KProperty1<out Any, *>.accessAndAnalyze(
        root: Root<*>,
        query: Any,
        cb: CriteriaBuilder,
        list: ArrayList<Predicate>,
        joins: MutableMap<String, Join<*, *>>
    ) {
        val accessible = this.isAccessible
        this.isAccessible = true
        try {
            this.javaField?.let { javaField ->
                val q = javaField.getAnnotation(Query::class.java) ?: return@let
                val value = javaField.get(query)
                val nullCondition = q.type == Query.Type.IS_NULL || q.type == Query.Type.NOT_NULL
                // IS_NULL/NOT_NULL 使用 Boolean 字段作为开关：true 才追加条件。
                val shouldAnalyze = if (nullCondition) {
                    value != null && value != false
                } else {
                    value.hasValue()
                }
                if (shouldAnalyze) {
                    analyzeQuery(root, cb, list, this, q, value, joins)
                }
            } ?: run {
                log.error("JavaField of KProperty1<out Any, *> object is null!")
            }
        } finally {
            this.isAccessible = accessible
        }
    }

    private fun analyzeQuery(
        root: Root<*>, cb: CriteriaBuilder, list: ArrayList<Predicate>, kProperty1: KProperty1<out Any, *>,
        q: Query, value: Any?, joins: MutableMap<String, Join<*, *>>
    ) {
        val nullCondition = q.type == Query.Type.IS_NULL || q.type == Query.Type.NOT_NULL
        if (!nullCondition && q.blurry.isNotBlank()) {
            proceedBlurry(root, cb, list, q.blurry, value ?: return)
        } else {
            defineAttrNameAndProceedFurther(root, cb, list, kProperty1, q, value, joins)
        }
    }

    private fun proceedBlurry(
        root: Root<*>,
        cb: CriteriaBuilder,
        list: ArrayList<Predicate>,
        blurry: String,
        value: Any
    ) {
        val blurryFields = blurry.split(",").map { it.trim() }.filter { it.isNotEmpty() }
        if (blurryFields.isEmpty()) {
            return
        }
        val orPredicate = blurryFields.map { cb.like(root.get<Any>(it).`as`(String::class.java), "%$value%") }
        list.add(cb.or(*orPredicate.toTypedArray()))
    }

    private fun defineAttrNameAndProceedFurther(
        root: Root<*>, cb: CriteriaBuilder, list: ArrayList<Predicate>, kProperty1: KProperty1<out Any, *>,
        q: Query, value: Any?, joins: MutableMap<String, Join<*, *>>
    ) {
        val attributeName = q.propName.takeIf { it.isNotBlank() } ?: kProperty1.name
        val join = analyzeJoinType(root, q, joins)
        proceedQueryType(root, cb, list, kProperty1, q, attributeName, join, value)
    }

    @Suppress("UNCHECKED_CAST")
    private fun analyzeJoinType(root: Root<*>, q: Query, joins: MutableMap<String, Join<*, *>>): Join<*, *>? {
        val joinName = q.joinName.trim()
        if (joinName.isEmpty()) {
            return null
        }

        var join: Join<*, *>? = null
        var path = ""
        for (entity in joinName.split(">").map { it.trim() }.filter { it.isNotEmpty() }) {
            path = if (path.isEmpty()) entity else "$path>$entity"
            val cached = joins[path]
            if (cached != null) {
                join = cached
                continue
            }
            if (join == null) {
                join = root.joins.firstOrNull { it.attribute.name == entity }
            }
            if (join == null) {
                join = root.join<Any, Any>(entity, q.join.toJoinType())
            } else if (path != entity) {
                join = (join as Join<Any, Any>).join<Any, Any>(entity, q.join.toJoinType())
            }
            joins[path] = join
        }
        return join
    }

    private fun Query.Join.toJoinType(): JoinType = when (this) {
        Query.Join.LEFT -> JoinType.LEFT
        Query.Join.RIGHT -> JoinType.RIGHT
        Query.Join.INNER -> JoinType.INNER
    }

    private fun proceedQueryType(
        root: Root<*>, cb: CriteriaBuilder, list: ArrayList<Predicate>, kProperty1: KProperty1<out Any, *>, q: Query,
        attributeName: String, join: Join<*, *>?, value: Any?
    ) {
        val fieldType = kProperty1.javaField?.type
            ?: throw IllegalArgumentException("无法获取查询字段类型: ${kProperty1.name}")
        val comparableFieldType = value?.castToComparableFieldType()
        val baseExpression = getExpression(attributeName, join, root)
        when (q.type) {
            Query.Type.EQUAL -> list.add(cb.equal(baseExpression.`as`(fieldType), value))
            Query.Type.NOT_EQUAL -> list.add(cb.notEqual(baseExpression, value))

            Query.Type.GREATER_THAN -> {
                val comparable = requireNotNull(value) { "GREATER_THAN 需要非空值: $attributeName" }
                list.add(
                    cb.greaterThanOrEqualTo(
                        baseExpression.`as`(comparableFieldType),
                        comparable.castToComparable(comparableFieldType)
                    )
                )
            }

            Query.Type.LESS_THAN -> {
                val comparable = requireNotNull(value) { "LESS_THAN 需要非空值: $attributeName" }
                list.add(
                    cb.lessThanOrEqualTo(
                        baseExpression.`as`(comparableFieldType),
                        comparable.castToComparable(comparableFieldType)
                    )
                )
            }

            Query.Type.LESS_THAN_NQ -> {
                val comparable = requireNotNull(value) { "LESS_THAN_NQ 需要非空值: $attributeName" }
                list.add(
                    cb.lessThan(
                        baseExpression.`as`(comparableFieldType),
                        comparable.castToComparable(comparableFieldType)
                    )
                )
            }

            Query.Type.INNER_LIKE -> list.add(
                createStringLikePredicate(cb, baseExpression, requireNotNull(value) { "INNER_LIKE 需要非空值: $attributeName" })
            )
            Query.Type.LEFT_LIKE -> list.add(
                createStringLikePredicate(cb, baseExpression, requireNotNull(value) { "LEFT_LIKE 需要非空值: $attributeName" }, true, false)
            )
            Query.Type.RIGHT_LIKE -> list.add(
                createStringLikePredicate(cb, baseExpression, requireNotNull(value) { "RIGHT_LIKE 需要非空值: $attributeName" }, false, true)
            )
            Query.Type.LIKE_STR -> list.add(
                cb.like(baseExpression.`as`(String::class.java), requireNotNull(value) { "LIKE_STR 需要非空值: $attributeName" }.toString())
            )
            Query.Type.IN_INNER_LIKE -> {
                if (value is List<*> && value.isNotEmpty()) {
                    // 构建数组
                    val predicates = arrayOfNulls<Predicate>(value.size)
                    for (i in value.indices) {
                        val obj = value[i]?.toString() ?: "null"
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
                if (value is List<*> && value.isNotEmpty()) {
                    require(value.size == 2) { "BETWEEN 需要两个值: $attributeName" }
                    val start = value[0]
                    val end = value[1]
                    if (start !is Comparable<*> || end !is Comparable<*>) {
                        throw IllegalArgumentException("BETWEEN 的值必须实现 Comparable: $attributeName")
                    }
                    val eleType = start.castToComparableFieldType()
                        ?: throw IllegalArgumentException("BETWEEN 字段不是 Comparable: $attributeName")
                    list.add(
                        cb.between(
                            baseExpression.`as`(eleType),
                            start.castToComparable(eleType),
                            end.castToComparable(eleType)
                        )
                    )
                }
            }

            Query.Type.NOT_NULL -> list.add(cb.isNotNull(baseExpression))
            Query.Type.IS_NULL -> list.add(cb.isNull(baseExpression))

            Query.Type.IN_OR_ISNULL -> {
                if (value is Collection<*> && value.isNotEmpty()) {
                    list.add(
                        cb.or(
                            baseExpression.`in`(value),
                            cb.isNull(baseExpression),
                            cb.equal(baseExpression.`as`(String::class.java), "")
                        )
                    )
                }
            }

            Query.Type.IS_OR_NULL -> {
                val isNull = value is Number && value.toLong() == -1L
                val expression = baseExpression.`as`(fieldType)
                list.add(if (isNull) cb.isNull(expression) else cb.equal(expression, value))
            }

            Query.Type.EQUAL_IN_MULTI -> {
                val actualValue = requireNotNull(value) { "EQUAL_IN_MULTI 需要非空值: $attributeName" }
                val predicates = arrayOfNulls<Predicate>(4)
                // like val
                predicates[0] = createStringLikePredicate(cb, baseExpression, "$actualValue", false, false)
                // like val,%
                predicates[1] = createStringLikePredicate(cb, baseExpression, "$actualValue,", false, true)
                // like %,val,%
                predicates[2] = createStringLikePredicate(cb, baseExpression, ",$actualValue,")
                // like %,val
                predicates[3] = createStringLikePredicate(cb, baseExpression, ",$actualValue", true, false)
                // 设置查询
                list.add(cb.or(*predicates))
            }

            Query.Type.FUNCTION_FIND_IN_SET -> {
                val actualValue = requireNotNull(value) { "FUNCTION_FIND_IN_SET 需要非空值: $attributeName" }
                list.add(
                    cb.greaterThan(
                        cb.function(
                            "FIND_IN_SET",
                            Int::class.java,
                            cb.literal(actualValue.toString()),
                            baseExpression
                        ), 0
                    )
                )
            }

            Query.Type.FUNCTION_4_EQUAL -> {
                require(q.functionName.isNotBlank()) { "FUNCTION_4_EQUAL 必须配置 functionName: $attributeName" }
                list.add(
                    cb.equal(
                        cb.function(
                            q.functionName,
                            fieldType,
                            baseExpression
                        ), value
                    )
                )
            }
        }
    }

    private fun Any?.hasValue() = this != null && (this !is String || this.trim() != "")

    private fun getExpression(attributeName: String, join: Join<*, *>?, root: Root<*>) =
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
            : Comparable<Any> = requireNotNull(fieldType) { "查询值不是 Comparable 类型" }.cast(this)

    private fun String?.isBlank(): Boolean = this == null || this.trim() == ""

}
