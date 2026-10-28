/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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
import jakarta.persistence.criteria.CriteriaBuilder
import jakarta.persistence.criteria.Expression
import jakarta.persistence.criteria.Join
import jakarta.persistence.criteria.JoinType
import jakarta.persistence.criteria.Path
import jakarta.persistence.criteria.Predicate
import jakarta.persistence.criteria.Root
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.mockito.Mockito
import org.mockito.Mockito.mock
import org.mockito.Mockito.never
import org.mockito.Mockito.verify
import org.mockito.Mockito.`when`

class KotlinQueryHelpTest {

    private class NullConditionCriteria {
        @Query(type = Query.Type.IS_NULL)
        var name: Boolean? = null

        @Query(type = Query.Type.NOT_NULL)
        var email: Boolean? = null
    }

    private class IsOrNullCriteria {
        @Query(type = Query.Type.IS_OR_NULL)
        var status: Int? = -1
    }

    private class BetweenCriteria {
        @Query(type = Query.Type.BETWEEN)
        var range: List<Int>? = listOf(1, 2)
    }

    private class FunctionCriteria {
        @Query(type = Query.Type.FUNCTION_4_EQUAL)
        var name: String? = "admin"
    }

    @Test
    fun `null conditions are evaluated with null field values`() {
        val root = mock(Root::class.java) as Root<Any>
        val cb = mock(CriteriaBuilder::class.java, Mockito.RETURNS_MOCKS)
        val namePath = mock(Path::class.java) as Path<Any>
        val emailPath = mock(Path::class.java) as Path<Any>
        val predicate = mock(Predicate::class.java)

        `when`(root.get<Any>("name")).thenReturn(namePath)
        `when`(root.get<Any>("email")).thenReturn(emailPath)
        `when`(cb.isNull(namePath)).thenReturn(predicate)
        `when`(cb.isNotNull(emailPath)).thenReturn(predicate)

        QueryHelp.getPredicate(root, NullConditionCriteria().apply {
            name = true
            email = true
        }, cb)

        verify(cb).isNull(namePath)
        verify(cb).isNotNull(emailPath)
    }

    @Test
    fun `is or null accepts integer minus one`() {
        val root = mock(Root::class.java) as Root<Any>
        val cb = mock(CriteriaBuilder::class.java, Mockito.RETURNS_MOCKS)
        val path = mock(Path::class.java) as Path<Any>
        @Suppress("UNCHECKED_CAST")
        val expression = path as Expression<java.lang.Integer>
        val predicate = mock(Predicate::class.java)

        `when`(root.get<Any>("status")).thenReturn(path)
        `when`(path.`as`(Integer::class.java)).thenReturn(expression)
        `when`(cb.isNull(expression)).thenReturn(predicate)

        QueryHelp.getPredicate(root, IsOrNullCriteria(), cb)

        verify(cb).isNull(expression)
        verify(cb, never()).equal(Mockito.any(), Mockito.any())
    }

    @Test
    fun `between accepts two comparable values`() {
        val root = mock(Root::class.java) as Root<Any>
        val cb = mock(CriteriaBuilder::class.java, Mockito.RETURNS_MOCKS)
        val path = mock(Path::class.java) as Path<Any>
        @Suppress("UNCHECKED_CAST")
        val expression = path as Expression<java.lang.Integer>

        `when`(root.get<Any>("range")).thenReturn(path)
        `when`(path.`as`(Integer::class.java)).thenReturn(expression)

        // 只要不抛异常即可；具体 predicate 的拼接由 Java 侧 QueryHelpTest 覆盖
        QueryHelp.getPredicate(root, BetweenCriteria(), cb)
    }

    @Test
    fun `function equal requires function name`() {
        val root = mock(Root::class.java) as Root<Any>
        val cb = mock(CriteriaBuilder::class.java, Mockito.RETURNS_MOCKS)
        val path = mock(Path::class.java) as Path<Any>

        `when`(root.get<Any>("name")).thenReturn(path)

        val exception = assertThrows(IllegalStateException::class.java) {
            QueryHelp.getPredicate(root, FunctionCriteria(), cb)
        }
        assertTrue(exception.cause?.message.orEmpty().contains("functionName"), exception.cause?.message)
    }

    @Test
    fun `same join path is reused within one query`() {
        class JoinCriteria {
            @Query(propName = "code", type = Query.Type.EQUAL, joinName = "roles")
            var roleCode: String? = "admin"

            @Query(propName = "level", type = Query.Type.GREATER_THAN, joinName = "roles")
            var roleLevel: Long? = 1L
        }

        val root = mock(Root::class.java) as Root<Any>
        val cb = mock(CriteriaBuilder::class.java, Mockito.RETURNS_MOCKS)
        val join = mock(Join::class.java) as Join<Any, Any>
        val codePath = mock(Path::class.java) as Path<Any>
        val levelPath = mock(Path::class.java) as Path<Any>

        `when`(root.joins).thenReturn(mutableSetOf())
        `when`(root.join<Any, Any>("roles", JoinType.LEFT)).thenReturn(join)
        `when`(join.get<Any>("code")).thenReturn(codePath)
        `when`(join.get<Any>("level")).thenReturn(levelPath)

        QueryHelp.getPredicate(root, JoinCriteria(), cb)

        verify(root).join<Any, Any>("roles", JoinType.LEFT)
    }
}
