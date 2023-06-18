/*
 * Copyright 2016-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license.
 */

// This file was automatically generated from channels.md by Knit tool. Do not edit.
package com.unicorn.learning

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

fun main3() = runBlocking {
    val channel = Channel<Int>(2)
    launch {
        for (x in 1..5) channel.send(x * x)
        channel.close() // we're done sending
    }
    // here we print received values using `for` loop (until the channel is closed)
    for (y in channel) println(y)
    println("Done!")
}

// print Prime Number
// 这个还需要理解，它的执行是这样的：numbersFrom(2) -> filter(2) -> filter(3) -> filter(5) -> filter(7) ...、
// 在这段代码中，numbersFrom 函数创建了一个初始的通道 cur，它会生成从参数 start 开始的整数流。
//  每次循环中，cur.receive() 从通道 cur 中接收一个素数。然后，通过调用 filter 函数，会创建一个新的通道，并将不被当前素数整除的数发送到新的通道中。这个新的通道成为下一次循环中的 cur，用于生成下一个素数。
//  这样，每次循环都会通过 filter 函数创建一个新的通道，并将不被当前素数整除的数发送到新的通道中，形成了一个过滤链。每个过滤器只保留被当前素数整除的数，然后将剩余的数发送到下一个过滤器。
//  因此，通过这种方式，你可以实现一个无限生成素数的流程，每次循环都会生成下一个素数，并将过滤后的新的整数流传递给下一个循环。
fun main() = runBlocking {
    var cur = numbersFrom(2)
    repeat(10) {
        val prime = cur.receive()
        println(prime)
        cur = filter(cur, prime)
    }
    coroutineContext.cancelChildren() // cancel all children to let main finish
}

@OptIn(ExperimentalCoroutinesApi::class)
fun CoroutineScope.numbersFrom(start: Int) = produce {
    var x = start
    while (true) send(x++) // infinite stream of integers from start
}

@OptIn(ExperimentalCoroutinesApi::class)
fun CoroutineScope.filter(numbers: ReceiveChannel<Int>, prime: Int) = produce {
    for (x in numbers) if (x % prime != 0) send(x)
}
