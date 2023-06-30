/*
 *    Copyright (c) 2023.  lWoHvYe(Hongyan Wang)
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
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.*;
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
}
