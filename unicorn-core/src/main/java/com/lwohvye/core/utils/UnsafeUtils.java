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

import lombok.experimental.UtilityClass;
import sun.misc.Unsafe;

/**
 * <a href="https://github.com/alibaba/fastjson2/blob/main/core/src/main/java/com/alibaba/fastjson2/util/UnsafeUtils.java">...</a>
 */
@UtilityClass
@SuppressWarnings("unused")
public class UnsafeUtils {
    public static final Unsafe UNSAFE;
    public static final long ARRAY_BYTE_BASE_OFFSET;

    static {
        Unsafe unsafe = null;
        var offset = -1;
        try {
            // 这里只能这样获取，通过Unsafe.getUnsafe()直接获取会抛出异常 if (!VM.isSystemDomainLoader(caller.getClassLoader()))
            var theUnsafeField = Unsafe.class.getDeclaredField("theUnsafe");
//            var theUnsafeField = Unsafe.class.getDeclaredFields()[0];
            theUnsafeField.trySetAccessible();
            unsafe = (Unsafe) theUnsafeField.get(null);
            offset = Unsafe.ARRAY_BYTE_BASE_OFFSET;
        } catch (Exception ignored) {
            // ignored
        }
        UNSAFE = unsafe;
        ARRAY_BYTE_BASE_OFFSET = offset;
    }
}
