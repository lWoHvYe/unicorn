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

package com.unicorn;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.concurrent.ForkJoinPool;

public class CustomParallelismTest {

    // Global configure ForkJoinPool
    // -Djava.util.concurrent.ForkJoinPool.common.parallelism=8

    @Test
    void testCustomThreadPool() {
        var largeDataSet = new ArrayList<>();
        largeDataSet.add("1");
        largeDataSet.add(2);

        try (var customThreadPool = new ForkJoinPool(16)) {
            customThreadPool.submit(() ->
                    largeDataSet.parallelStream().forEach(System.out::println));
        }
    }
}
