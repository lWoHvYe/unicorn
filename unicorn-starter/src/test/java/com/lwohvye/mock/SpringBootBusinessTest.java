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

package com.lwohvye.mock;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.Arrays;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

// https://www.lwohvye.com/2022/05/12/%e6%a8%a1%e6%8b%9f%e6%a1%86%e6%9e%b6mockito/
@SpringBootTest // 默认就是MOCK
class SpringBootBusinessTest {
    @MockBean // 使用SpringExtension时，使用MockBean注解来mock bean，这个跟mock是在不同的project中的
    DataService dataService; // Mock似乎是没有deep限制的，比如 UT -> A -> B,C -> D 完全可以对B以及对D进行Mock，这样UT有了更多的设计空间
    @Autowired
    SomeBusiness business;

    @Test
    void testSpringMock() {
        when(dataService.getAll(anyString())).thenReturn(Arrays.asList("1", "2", "3")); // 主体是对需要mock的operator先进行define （stub）。可以mock与remote API, 3rd-Sys, local的 interact
        Assertions.assertEquals(Arrays.asList("1", "2", "3"), business.fetchAllData(anyString()));
    }
}
