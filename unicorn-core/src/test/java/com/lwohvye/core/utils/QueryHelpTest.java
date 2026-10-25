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

package com.lwohvye.core.utils;

import com.lwohvye.core.annotation.Query;
import com.lwohvye.core.exception.UtilsException;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class QueryHelpTest {

    static class IsOrNullCriteria {
        @Query(type = Query.Type.IS_OR_NULL)
        Integer status;
    }

    static class BetweenCriteria {
        @Query(type = Query.Type.BETWEEN)
        List<Integer> range;
    }

    static class InvalidBetweenCriteria {
        @Query(type = Query.Type.BETWEEN)
        List<Object> range;
    }

    static class InnerLikeCriteria {
        @Query(type = Query.Type.IN_INNER_LIKE)
        List<String> values;
    }

    static class FunctionCriteria {
        @Query(type = Query.Type.FUNCTION_4_EQUAL)
        String name;
    }

    static class JoinCriteria {
        @Query(propName = "code", type = Query.Type.EQUAL, joinName = "roles")
        String roleCode;

        @Query(propName = "level", type = Query.Type.GREATER_THAN, joinName = "roles")
        Long roleLevel;
    }

    static class NullConditionCriteria {
        @Query(type = Query.Type.IS_NULL)
        String name;

        @Query(type = Query.Type.NOT_NULL)
        String email;
    }

    @Test
    void isOrNullAcceptsIntegerMinusOne() {
        var criteria = new IsOrNullCriteria();
        criteria.status = -1;

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var path = mock(Path.class);
        var predicate = mock(Predicate.class);
        when(root.get("status")).thenReturn(path);
        when(path.as(Integer.class)).thenReturn(path);
        when(cb.isNull(path)).thenReturn(predicate);
        when(cb.and(any(Predicate[].class))).thenReturn(predicate);

        assertSame(predicate, QueryHelp.getPredicate(root, criteria, cb));
        verify(cb).isNull(path);
        verify(cb, never()).equal(any(), any());
    }

    @Test
    void isOrNullUsesEqualForNormalValue() {
        var criteria = new IsOrNullCriteria();
        criteria.status = 5;

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var path = mock(Path.class);
        var predicate = mock(Predicate.class);
        when(root.get("status")).thenReturn(path);
        when(path.as(Integer.class)).thenReturn(path);
        when(cb.equal(path, 5)).thenReturn(predicate);
        when(cb.and(any(Predicate[].class))).thenReturn(predicate);

        assertSame(predicate, QueryHelp.getPredicate(root, criteria, cb));
        verify(cb).equal(path, 5);
        verify(cb, never()).isNull(any());
    }

    @Test
    void betweenCastsBothBounds() {
        var criteria = new BetweenCriteria();
        criteria.range = List.of(1, 2);

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var path = mock(Path.class);
        var predicate = mock(Predicate.class);
        when(root.get("range")).thenReturn(path);
        when(path.as(Integer.class)).thenReturn(path);
        when(cb.between(path, 1, 2)).thenReturn(predicate);
        when(cb.and(any(Predicate[].class))).thenReturn(predicate);

        assertSame(predicate, QueryHelp.getPredicate(root, criteria, cb));
        verify(cb).between(path, 1, 2);
    }

    @Test
    void betweenRejectsNonComparableValues() {
        var criteria = new InvalidBetweenCriteria();
        criteria.range = List.of(new Object(), new Object());

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var path = mock(Path.class);
        when(root.get("range")).thenReturn(path);

        var exception = assertThrows(UtilsException.class,
                () -> QueryHelp.getPredicate(root, criteria, cb));
        assertTrue(exception.getMessage().contains("BETWEEN"));
    }

    @Test
    void emptyInnerLikeDoesNotCreateEmptyOrPredicate() {
        var criteria = new InnerLikeCriteria();
        criteria.values = List.of();

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var predicate = mock(Predicate.class);
        when(cb.and(any(Predicate[].class))).thenReturn(predicate);

        assertSame(predicate, QueryHelp.getPredicate(root, criteria, cb));
        verify(cb, never()).or(any(Predicate[].class));
    }

    @Test
    void functionEqualRequiresFunctionName() {
        var criteria = new FunctionCriteria();
        criteria.name = "admin";

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var path = mock(Path.class);
        when(root.get("name")).thenReturn(path);

        var exception = assertThrows(UtilsException.class,
                () -> QueryHelp.getPredicate(root, criteria, cb));
        assertTrue(exception.getMessage().contains("functionName"));
    }

    @Test
    void nullConditionsWorkWithNullFieldValues() {
        var criteria = new NullConditionCriteria();

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var namePath = mock(Path.class);
        var emailPath = mock(Path.class);
        var predicate = mock(Predicate.class);
        when(root.get("name")).thenReturn(namePath);
        when(root.get("email")).thenReturn(emailPath);
        when(cb.isNull(namePath)).thenReturn(predicate);
        when(cb.isNotNull(emailPath)).thenReturn(predicate);
        when(cb.and(any(Predicate[].class))).thenReturn(predicate);

        assertSame(predicate, QueryHelp.getPredicate(root, criteria, cb));
        verify(cb).isNull(namePath);
        verify(cb).isNotNull(emailPath);
    }

    @Test
    void sameJoinPathIsReusedWithinOneQuery() {
        var criteria = new JoinCriteria();
        criteria.roleCode = "admin";
        criteria.roleLevel = 1L;

        var root = mock(Root.class);
        var cb = mock(CriteriaBuilder.class);
        var join = mock(Join.class);
        var codePath = mock(Path.class);
        var levelPath = mock(Path.class);
        var predicate = mock(Predicate.class);

        when(root.getJoins()).thenReturn(Collections.emptySet());
        when(root.join("roles", JoinType.LEFT)).thenReturn(join);
        when(join.get("code")).thenReturn(codePath);
        when(join.get("level")).thenReturn(levelPath);
        when(codePath.as(String.class)).thenReturn(codePath);
        when(levelPath.as(Long.class)).thenReturn(levelPath);
        when(cb.and(any(Predicate[].class))).thenReturn(predicate);

        assertSame(predicate, QueryHelp.getPredicate(root, criteria, cb));
        verify(root).join("roles", JoinType.LEFT);
    }
}
