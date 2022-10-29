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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class) // 这种不会start一个SpringBoot Container，粒度更细，针对在Start时会connect db, cache, mq这些，然后UT run 的 env 又没有的情景，可以用这种
class TestTutorialApplicationTests {
    @Mock //使用MockitoExtension时，使用Mock注解来mock bean
    DataService dataService;
    @InjectMocks //非mock的bean用InjectMocks来Inject。算是用mock自己的那套体系
    SomeBusiness someBusiness;

    // @BeforeAll
    // @BeforeEach
    // @AfterAll
    // @AfterEach

    @Test
    void testFetchAll() {
        // 定义当调用指定方法时的，Mock值 stub
        // 对于带参数的，可以用anyString()、anyInt()这些
        when(dataService.getAll(anyString())).thenReturn(Arrays.asList("1", "2", "3"));
        Assertions.assertEquals(Arrays.asList("1", "2", "3"), someBusiness.fetchAllData(anyString()));
    }

    // 参数化测试注释。不加报错org.junit.jupiter.api.extension.ParameterResolutionException: No ParameterResolver registered for parameter [xx] in method xxx
    @ParameterizedTest(name = "汉:{0},数:{1}") // name这块应用在显示结果那里
    // 数据源。另有注解从文件中读取
    @CsvSource({
            "one,1",
            "two,2",
            "three,3",
            "zero,0"
    })
    void testStatic(String s, Integer i) {
        System.out.println(StaticBS.pr(s) + i); // 原值Local
        try (var bsMockedStatic = Mockito.mockStatic(StaticBS.class)) { // 这个得关闭
            // 这里支持   when(StaticBS::pr) 和 when(() -> StaticBS.pr(s)) 两种方式
            bsMockedStatic.when(() -> StaticBS.pr(s)).thenReturn("First Try").thenReturn("Last Try").thenCallRealMethod().thenThrow(new RuntimeException("别调了"));
            System.out.println(StaticBS.pr(s)); // First Try
            System.out.println(StaticBS.pr(s)); // Last Try
            System.out.println(StaticBS.pr(s)); // 原值Local
            System.out.println(StaticBS.pr(s)); // 抛出异常
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }

}
