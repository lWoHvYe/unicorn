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

public class StringTemplatesTest {
    // 这个只是语法糖，并不是Engine
    @Test
    void test1() {
        int x = 10, y = 20;
        String s = STR. "\{ x } + \{ y } = \{ x + y }" ;
        System.out.println("s = " + s);
        String[] fruit = {"apples", "oranges", "peaches"};
        String s2 = STR. "\{ fruit[0] }, \{ STR. "\{ fruit[1] }, \{ fruit[2] }" }" ;
        System.out.println("s2 = " + s2);
        String tmp = STR. "\{ fruit[1] }, \{ fruit[2] }" ;
        String s3 = STR. "\{ fruit[0] }, \{ tmp }" ;
        System.out.println("s3 = " + s3);
        String s4 = STR. "\{ fruit[0] }, \{ fruit[1] }, \{ fruit[2] }" ;
        System.out.println("s4 = " + s4);

        var s5 = "1 + 2";
        System.out.println(STR. "\{ s5 + 1 + 2 }" ); // 1 + 212

    }
}
