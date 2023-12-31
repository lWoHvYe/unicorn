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

package com.unicorn.learning

import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

fun simple(): Flow<String> = flow {
    for (i in 1..3) {
        //Thread.sleep(100) // pretend we are computing it in CPU-consuming way
        log("Emitting $i")
        emit(i) // emit next value
    }
}.map { "string-$it" }.flowOn(Dispatchers.Default) // RIGHT way to change context for CPU-consuming code in flow builder

fun main2() = runBlocking {
    simple()
        .buffer() // buffer emissions, don't wait
        // The catch intermediate operator, honoring exception transparency, catches only upstream exceptions (that is an exception from all the operators above catch, but not below it).
        // If the block in collect { ... } (placed below catch) throws an exception then it escapes:
        .catch { e -> emit("Caught $e") } // emit on exception
        .collect { value ->
            log("Collected $value")
        }
}

fun main1() = runBlocking {
    simple()
        // We can combine the declarative nature of the catch operator with a desire to handle all the exceptions,
        // by moving the body of the collect operator into onEach and putting it before the catch operator.
        .onEach { value ->
            check(value.contains("2")) { "Collected $value" }
            println(value)
        }
        .onCompletion { cause -> if (cause != null) println("Flow completed exceptionally") }
        .catch { e -> println("Caught $e") }
        .collect()
}

// Imitate a flow of events
fun events(): Flow<Int> = (1..3).asFlow().onEach { delay(100) }

fun main0() = runBlocking {
    events()
        .onEach { event -> println("Event: $event") }
        .launchIn(this) // <--- Launching the flow in a separate coroutine
    println("Done")
}

fun main() = runBlocking<Unit> {
    // For convenience, the flow builder performs additional ensureActive checks for cancellation on each emitted value.
    // It means that a busy loop emitting from a flow { ... } is cancellable: such as `events()`
    // But if you use IntRange.asFlow extension to write the same busy loop and don't suspend anywhere, then there are no checks for cancellation:
    // So we should use the `cancellable` operator to make busy flow cancellable
    (1..5).asFlow().cancellable().collect { value ->
        if (value == 3) cancel()
        println(value)
    }
}

fun log(msg: String) = println("[${Thread.currentThread().name}] $msg")
