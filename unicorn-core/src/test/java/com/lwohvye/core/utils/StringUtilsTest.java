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

package com.lwohvye.core.utils;

import com.lwohvye.core.extension.StringExtensionMethod;
import lombok.experimental.ExtensionMethod;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;

import java.text.SimpleDateFormat;
import java.util.Date;

import static com.lwohvye.core.utils.StringUtils.getIp;
import static com.lwohvye.core.utils.StringUtils.getWeekDay;
import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtensionMethod({StringExtensionMethod.class})
class StringUtilsTest {

    @Test
    void testToCapitalizeCamelCase() {
        assertEquals("HelloWorld", "hello_world".toCapitalizeCamelCase());
    }

    @Test
    void testToUnderScoreCase() {
//        assertNull(StringUtils.toUnderScoreCase(null));
        assertEquals("hello_world", "helloWorld".toUnderScoreCase());
        assertEquals("\u0000\u0000", "\u0000\u0000".toUnderScoreCase());
        assertEquals("\u0000_a", "\u0000A".toUnderScoreCase());
    }

    @Test
    void testGetWeekDay() {
        SimpleDateFormat simpleDateformat = new SimpleDateFormat("E");
        assertEquals(simpleDateformat.format(new Date()), getWeekDay());
    }

    @Test
    void testGetIP() {
        assertEquals("127.0.0.1", getIp(new MockHttpServletRequest()));
    }
}
