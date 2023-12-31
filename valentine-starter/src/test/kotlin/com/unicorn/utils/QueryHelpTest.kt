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

import com.lwohvye.api.modules.system.domain.User
import com.lwohvye.core.utils.SpringContextHolder
import com.lwohvye.sys.modules.system.repository.UserRepository
import com.unicorn.dto.UserQueryCriteria
import jakarta.persistence.criteria.CriteriaBuilder
import jakarta.persistence.criteria.CriteriaQuery
import jakarta.persistence.criteria.Root
import org.junit.jupiter.api.Test
import org.springframework.boot.test.context.SpringBootTest

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class QueryHelpTest {

    @Test
    fun getPredicate() {
        val criteria = UserQueryCriteria()
        criteria.id = 1L
        criteria.usernameStr = "a,b,c,d,e"
        criteria.deptIds = setOf(1L, 2L, 3L)
        criteria.blurry = "ABC"
        criteria.roleCode = "Admin"
        criteria.roleLevel = 2L
        criteria.roleDeptEnable = true
        val repository = SpringContextHolder.getBean(UserRepository::class.java)
        val page: List<User> =
            repository.findAll { root: Root<User>?, _: CriteriaQuery<*>?, criteriaBuilder: CriteriaBuilder? ->
                QueryHelp.getPredicate(
                    root!!,
                    criteria,
                    criteriaBuilder!!
                )
            }
        println("Kotlin ------- ${page.size}")
        val criteria2 = com.lwohvye.api.modules.system.service.dto.UserQueryCriteria()

        criteria2.id = 1L
        criteria2.usernameStr = "a,b,c,d,e"
        criteria2.deptIds = setOf(1L, 2L, 3L)
        criteria2.blurry = "ABC"
        criteria2.roleCode = "Admin"
        criteria2.roleLevel = 2L
        criteria2.roleDeptEnable = true
        val page2: List<User> =
            repository.findAll { root: Root<User>?, _: CriteriaQuery<*>?, criteriaBuilder: CriteriaBuilder? ->
                com.lwohvye.core.utils.QueryHelp.getPredicate(
                    root!!,
                    criteria2,
                    criteriaBuilder!!
                )
            }
        println("Java --------${page2.size}")
    }
}
