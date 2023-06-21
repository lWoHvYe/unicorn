/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.starter.modules.strategy

import com.lwohvye.sys.modules.system.annotation.UserTypeHandlerAnno
import com.lwohvye.sys.modules.system.strategy.ExtraUserTypeStrategy
import kotlinx.coroutines.*
import org.springframework.security.core.GrantedAuthority
import org.springframework.stereotype.Component

@Component
@UserTypeHandlerAnno(typeName = "FOUR")
class TKMUserTypeStrategy : ExtraUserTypeStrategy {
    @OptIn(DelicateCoroutinesApi::class)
    override fun grantedAuth(userId: Long): List<GrantedAuthority> {

        println("Start")

        // kotlinx.coroutines将在1.7版本支持JPMS。当前报错 `module kotlin.stdlib does not read module kotlinx.coroutines.core.jvm`
        // https://github.com/Kotlin/kotlinx.coroutines/issues/2237
        // https://github.com/Kotlin/kotlinx.coroutines/pull/3297
        // 使用launch{}函数 启动一个协程
        GlobalScope.launch {
            runBlocking { // 在 runBlocking {} 包装中使用 delay，它启动了一个协程并等待直到它结束
                delay(1000)
            }
            println("Hello")
        }

        Thread.sleep(2000) // 等待 2 秒钟
        println("Stop")
        //_____________

        val result = GlobalScope.async {
            workload(16) // 调用函数
        }
        runBlocking {
            println(result.await())
        }

        return emptyList()
    }

    suspend fun workload(n: Int): Int { // 使用suspend修饰
        delay(1000)
        return n + 2
    }
}
