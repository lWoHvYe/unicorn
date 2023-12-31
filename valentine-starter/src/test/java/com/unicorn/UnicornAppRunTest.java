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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

//@SpringBootTest
@ExtendWith(SpringExtension.class)
class UnicornAppRunTest {

    @Disabled
    @Test
    void anyMatcher() {
        var any = any();
        // the below are all match String
        var anyString = anyString();
        var anyString2 = any(String.class);
        var anyString3 = ArgumentMatchers.<String>any();
        var anyString4 = isA(String.class);

        // the below are for Class<T>.class tClazz
        var anyClazz = any(Class.class); // Class.class
        var anyTypeStringClazz = ArgumentMatchers.<Class<String>>any(); // Class<String>.class


        var request = mock(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpMethod.GET.name());

        var response = mock(HttpServletResponse.class);
        when(response.getStatus()).thenReturn(HttpStatus.OK.value());


        var responseEntity = mock(ResponseEntity.class);
        when(responseEntity.getStatusCode()).thenReturn(HttpStatus.OK);
        when(responseEntity.getBody()).thenReturn("");

    }

    //    stub相同方法的不同调用次数时的返回值
    @Test
    void MultiCallReturnMethodStubTest() {
        var list = mock(List.class);
        when(list.size()).thenReturn(1, 2, 3, 4);
        assertEquals(1, list.size());
        assertEquals(2, list.size());
        assertEquals(3, list.size());
        assertEquals(4, list.size());
//      后续调用，最后的stub生效
        assertEquals(4, list.size());
        assertEquals(4, list.size());
        assertEquals(4, list.size());
        assertEquals(4, list.size());
    }

    //    调用mock对象由参数的方法时，需要根据参数来返回不同的返回值，需要thenAnswer来完成
    @Test
    void MultiCallArgMethodReturnStubTest() {
        var list = mock(List.class);
        when(list.get(anyInt())).thenAnswer(invocation -> {
            Integer argument = invocation.getArgument(0);
            return String.valueOf(argument * 10);
        });

        assertEquals("10", list.get(1));
        assertEquals("990", list.get(99));
    }
}
