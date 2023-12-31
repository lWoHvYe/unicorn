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

package com.lwohvye.core.extension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class StringExtensionMethod {

    private static final char SEPARATOR = '_';

    /**
     * 驼峰命名法工具
     *
     * @return toCamelCase(" hello_world ") == "helloWorld"
     * toCapitalizeCamelCase("hello_world") == "HelloWorld"
     * toCapitalizeCamelCase("helloWorld") == "HelloWorld"  下划线转驼峰,且首字母转大写
     * toUnderScoreCase("helloWorld") = "hello_world"
     * toUnderScoreCase("HelloWorld") = "hello_world"  驼峰转下划线，且首字母转小写
     */
    public static String toCamelCase(String s) {
        if (s == null) {
            return null;
        }

//        先统一转了小写。如果传个驼峰的进来，就会把驼峰转成小写，所以移除掉
//        s = s.toLowerCase();

        StringBuilder sb = new StringBuilder(s.length());
        boolean upperCase = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);

            if (c == SEPARATOR) {
                upperCase = true;
            } else if (upperCase) {
                sb.append(Character.toUpperCase(c));
                upperCase = false;
            } else {
                sb.append(c);
            }
        }

        return sb.toString();
    }

    /**
     * 驼峰命名法工具
     *
     * @return toCamelCase(" hello_world ") == "helloWorld"
     * toCapitalizeCamelCase("hello_world") == "HelloWorld"
     * toUnderScoreCase("helloWorld") = "hello_world"
     */
    public static String toCapitalizeCamelCase(String s) {
        if (s == null) {
            return null;
        }
        s = toCamelCase(s);
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    /**
     * 驼峰命名法工具
     *
     * @return toCamelCase(" hello_world ") == "helloWorld"
     * toCapitalizeCamelCase("hello_world") == "HelloWorld"
     * toUnderScoreCase("helloWorld") = "hello_world"
     */
    public static String toUnderScoreCase(String s) {
        if (s == null) {
            return null;
        }

        StringBuilder sb = new StringBuilder();
        boolean upperCase = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);

            boolean nextUpperCase = true;

            if (i < (s.length() - 1)) {
                nextUpperCase = Character.isUpperCase(s.charAt(i + 1));
            }

            if ((i > 0) && Character.isUpperCase(c)) {
                if (!upperCase || !nextUpperCase) {
                    sb.append(SEPARATOR);
                }
                upperCase = true;
            } else {
                upperCase = false;
            }

            sb.append(Character.toLowerCase(c));
        }

        return sb.toString();
    }

    /**
     * 只把首字母转小写。
     *
     * @param s
     * @return java.lang.String
     * @date 2021/7/13 11:53 上午
     */
    public static String lowerFirstChar(String s) {
        if (s.isBlank())
            return s;
        var chars = s.toCharArray();
        if (Character.isUpperCase(chars[0]))
            chars[0] += 32;
        return String.valueOf(chars);
    }

    /**
     * 只把首字母转大写。
     *
     * @param s
     * @return java.lang.String
     * @date 2021/7/13 11:54 上午
     */
    public static String upperFirstChar(String s) {
        if (s.isBlank())
            return s;
        var chars = s.toCharArray();
        if (Character.isLowerCase(chars[0]))
            chars[0] -= 32;
        return String.valueOf(chars);
    }

    public static List<Long> parseStrToArrLong(String str) {
        return !str.isBlank() ? Arrays.stream(str.split(",")).map(Long::parseLong).toList() : new ArrayList<>();
    }

    public static List<Integer> parseStrToArrInteger(String str) {
        return !str.isBlank() ? Arrays.stream(str.split(",")).map(Integer::parseInt).toList() : new ArrayList<>();
    }

    public static List<String> parseStrToArrString(String str) {
        return !str.isBlank() ? Arrays.stream(str.split(",")).toList() : new ArrayList<>();
    }

}
