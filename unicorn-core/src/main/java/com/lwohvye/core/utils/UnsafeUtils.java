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

import java.lang.reflect.Field;

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

    public static Object getObject(Object o, long offset) {
        return UNSAFE.getObject(o, offset);
    }

    public static long getLong(Object o, long offset) {
        return UNSAFE.getLong(o, offset);
    }

    public static int getInt(Object o, long offset) {
        return UNSAFE.getInt(o, offset);
    }

    public static short getShort(Object o, long offset) {
        return UNSAFE.getShort(o, offset);
    }

    public static byte getByte(Object o, long offset) {
        return UNSAFE.getByte(o, offset);
    }

    public static float getFloat(Object o, long offset) {
        return UNSAFE.getFloat(o, offset);
    }

    public static double getDouble(Object o, long offset) {
        return UNSAFE.getDouble(o, offset);
    }

    public static boolean getBoolean(Object o, long offset) {
        return UNSAFE.getBoolean(o, offset);
    }

    public static char getChar(Object o, long offset) {
        return UNSAFE.getChar(o, offset);
    }

    public static void putObject(Object o, long offset, Object x) {
        UNSAFE.putObject(o, offset, x);
    }

    public static void putInt(Object o, long offset, int x) {
        UNSAFE.putInt(o, offset, x);
    }

    public static void putLong(Object o, long offset, long x) {
        UNSAFE.putLong(o, offset, x);
    }

    public static void putFloat(Object o, long offset, float x) {
        UNSAFE.putFloat(o, offset, x);
    }

    public static void putDouble(Object o, long offset, double x) {
        UNSAFE.putDouble(o, offset, x);
    }

    public static void putShort(Object o, long offset, short x) {
        UNSAFE.putShort(o, offset, x);
    }

    public static void putByte(Object o, long offset, byte x) {
        UNSAFE.putByte(o, offset, x);
    }

    public static void putChar(Object o, long offset, char x) {
        UNSAFE.putChar(o, offset, x);
    }

    public static void putBoolean(Object o, long offset, boolean x) {
        UNSAFE.putBoolean(o, offset, x);
    }

    public static Object allocateInstance(Class<?> cls) throws InstantiationException {
        return UNSAFE.allocateInstance(cls);
    }

    public static long objectFieldOffset(Field field) {
        return UNSAFE.objectFieldOffset(field);
    }
}
