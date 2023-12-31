/*
 *    Copyright (c) 2023-2024.  lWoHvYe(Hongyan Wang)
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

package com.unicorn.coroutine

import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Runnable
import java.util.concurrent.ThreadFactory
import kotlin.coroutines.CoroutineContext

class VirtualThreadCoroutineDispatcher(val immediate: Boolean = false) : CoroutineDispatcher() {

    companion object {
        val factory: ThreadFactory = Thread.ofVirtual().name("Virtual-Dispatcher").factory()
    }

    override fun isDispatchNeeded(context: CoroutineContext): Boolean =
        !(immediate && Thread.currentThread().isVirtual)

    override fun dispatch(context: CoroutineContext, block: Runnable) {
        // 下面三种方式都可以，Creates a virtual thread to execute a task and schedules it to execute，其中第一种的Thread没有name
//        Thread.startVirtualThread(block)
//        Thread.ofVirtual().name("Virtual-Dispatcher").start(block)
        factory.newThread(block).start()
    }
}
