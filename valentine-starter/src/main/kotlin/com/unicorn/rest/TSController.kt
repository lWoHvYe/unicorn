/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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
package com.unicorn.rest

import com.unicorn.service.ITSService
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.springframework.context.annotation.Profile
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import kotlin.time.Duration

@Profile("dev")
@RestController
@RequestMapping("/api/anonymous/tsScope")
class TSController(val itsService: ITSService) {

    private var name: String = ""

    @GetMapping(value = ["/{username}"])
    fun userProfile(@PathVariable("username") username: String) {

        name = username
        itsService.setField(name)
        runBlocking {
            repeat(5) {
                launch {
                    delay(Duration.parse("${it}s"))
                    println(" ${Thread.currentThread().threadId()} name: $name  --> tsName: ${itsService.outIn()}")
                }
            }
        }
    }
}
