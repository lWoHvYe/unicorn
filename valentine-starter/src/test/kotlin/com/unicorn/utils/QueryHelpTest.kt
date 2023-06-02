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
        criteria.setUsernameStr("a,b,c,d,e")
        val repository = SpringContextHolder.getBean(UserRepository::class.java)
        val page: List<User> =
            repository.findAll { root: Root<User>?, _: CriteriaQuery<*>?, criteriaBuilder: CriteriaBuilder? ->
                QueryHelp.getPredicate(
                    root!!,
                    criteria,
                    criteriaBuilder!!
                )
            }
        println("-------")
        val criteria2 = com.lwohvye.api.modules.system.service.dto.UserQueryCriteria()
        criteria2.usernameStr = "a,b,c,d,e"
        val page2: List<User> =
            repository.findAll { root: Root<User>?, _: CriteriaQuery<*>?, criteriaBuilder: CriteriaBuilder? ->
                com.lwohvye.core.utils.QueryHelp.getPredicate(
                    root!!,
                    criteria2,
                    criteriaBuilder!!
                )
            }
    }
}
