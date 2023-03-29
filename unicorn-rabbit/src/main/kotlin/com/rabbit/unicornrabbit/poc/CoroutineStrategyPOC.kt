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
package com.rabbit.unicornrabbit.poc

import jakarta.annotation.PostConstruct
import kotlinx.coroutines.*
import org.apache.logging.log4j.LogManager.getLogger
import org.jetbrains.annotations.BlockingExecutor
import org.springframework.stereotype.Component
import java.util.concurrent.Executors
import kotlin.time.ExperimentalTime
import kotlin.time.measureTime


@Component
class CoroutineStrategyPOC {

    @PostConstruct
    fun doTest() {
        grantedAuth(4)
        println("navigate into loop")
        for (i in 0..10) {
            loomCarrier()
            coroutinesDirect()
            println("loop $i")
        }
    }

    @OptIn(DelicateCoroutinesApi::class)
    fun grantedAuth(userId: Int): List<String> {

        println("Start")

        // kotlinx.coroutines将在1.7版本支持JPMS，但在1.7.0-Beta。若以DeBug模式启动，依旧报错 `module kotlin.stdlib does not read module kotlinx.coroutines.core`，Normal run正常
        // https://github.com/Kotlin/kotlinx.coroutines/issues/2237
        // https://github.com/Kotlin/kotlinx.coroutines/pull/3297
        //  Coroutines can perfectly benefit from Loom: A Coroutine always relies on a thread for its execution.
        //  This Thread can also be a VirtualThread. The advantage of having a VirtualThread executing a Coroutine is that all IO operations,
        //  concurrency locks etc. will behave in a non-blocking fashion, wasting no resources.
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

        loomCarrier()
        coroutinesDirect()
        //_____________

        return emptyList()
    }

    suspend fun workload(n: Int): Int { // 使用suspend修饰
        delay(1000)
        return n + 2
    }

    // Define a Custom Dispatcher
    // What if instead of blocking a regular thread, we run it on one of Project Loom’s virtual threads,
    // effectively turning the blocking code into something non-blocking while still being Coroutine compatible?
    val Dispatchers.LOOM: @BlockingExecutor CoroutineDispatcher
        get() = Executors.newVirtualThreadPerTaskExecutor().asCoroutineDispatcher()

    // use Loom. This will need some time to warm up, overhead. 7s for 1_000_000, Debug Mode 17s for 100_000，比coroutines慢了太多了
    @OptIn(ExperimentalTime::class)
    fun loomCarrier() = runBlocking {

        val log = getLogger()

        measureTime {
            supervisorScope {
                repeat(1_000_000) {
                    launch(Dispatchers.LOOM) {
                        Thread.sleep(1000)
                    }
                }
            }
        }.also(log::info)

        Unit

    }

    // use Kotlinx Coroutines. While this is ballpark. 4.5s for 1_000_000, Debug Mode 5.2s for 100_000
    @OptIn(ExperimentalTime::class)
    fun coroutinesDirect() = runBlocking {

        val log = getLogger()

        measureTime {
            supervisorScope {
                repeat(1_000_000) {
                    launch {
                        delay(1000)
                    }
                }
            }
        }.also(log::info)

        Unit

    }
}
