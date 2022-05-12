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

package com.lwohvye.mock;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class TestTutorialApplicationTests {
    @Mock
    DataService dataService;
    @InjectMocks
    SomeBusiness someBusiness;

    // @BeforeAll
    // @BeforeEach
    // @AfterAll
    // @AfterEach

    @Test
    public void testFetchAll() {
        // 定义当调用指定方法时的，Mock值 stub
        when(dataService.getAll()).thenReturn(Arrays.asList("1", "2", "3"));
        Assertions.assertEquals(Arrays.asList("1", "2", "3"), someBusiness.fetchAllData());
    }
}
