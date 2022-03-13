/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.designpatterns.decorator;

import org.junit.jupiter.api.Test;

public class DecTest {

    @Test
    public void testDec() {
        ActivityInterface activity = new Activity.Builder().build();
        activity.participate(1L);

        activity = new RiskControlDecorator(activity); // 第一层装饰
        activity = new DesertControlDecorator(activity); // 第二层装饰

        activity.participate(2L); // 装饰可以视作在外面包了一层，这里输出时，也是从外到内，先第二层、再第一层，最后原对象
    }
}
