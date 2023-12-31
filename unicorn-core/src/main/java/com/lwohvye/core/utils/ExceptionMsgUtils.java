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

public class ExceptionMsgUtils {

    /**
     * Msg: Entity with fieldName -> fieldVal is intro
     *
     * @date 2022/9/18 9:47 AM
     */
    public static String generateExcMsg(Class<?> entity, String fieldName, String fieldVal, String intro) {
        return String.format(" %s with %s-> %s is %s ", StringUtils.capitalize(entity.getSimpleName()), fieldName, fieldVal, intro);
    }


    public static String genUnComparableExcMsg(String fieldName) {
        return String.format("%s is UnComparable", fieldName);
    }
}
