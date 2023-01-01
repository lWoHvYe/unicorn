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

package com.lwohvye.core.utils;

import java.lang.invoke.*;
import java.lang.reflect.Field;
import java.nio.ByteOrder;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.*;

import static java.lang.invoke.MethodType.methodType;

/**
 * <a href="https://github.com/alibaba/fastjson2/blob/main/core/src/main/java/com/alibaba/fastjson2/util/JDKUtils.java">...</a>
 */
@SuppressWarnings({"unchecked", "unused"})
public class JDKUtils {
    public static final int JVM_VERSION;
    // JDK9之后，通过byte[]来保存value，通过code字段区分是LATIN1或者UTF16。大多数的字符串都是LATIN1。
    public static final Byte LATIN1 = 0;
    public static final Byte UTF16 = 1;

    static final Field FIELD_STRING_VALUE;
    static final long FIELD_STRING_VALUE_OFFSET;
    static volatile boolean FIELD_VALUE_STRING_ERROR;

    static final Class<?> CLASS_SQL_DATASOURCE;
    static final Class<?> CLASS_SQL_ROW_SET;
    public static final boolean HAS_SQL;
    public static final boolean ANDROID;
    public static final boolean GRAAL;

    // Android not support
    public static final Class<?> CLASS_TRANSIENT;
    public static final boolean BIG_ENDIAN;

    public static final boolean UNSAFE_SUPPORT;

    // GraalVM not support
    // Android not support
    public static final BiFunction<char[], Boolean, String> STRING_CREATOR_JDK8;
    public static final BiFunction<byte[], Byte, String> STRING_CREATOR_JDK11;
    public static final ToIntFunction<String> STRING_CODER;
    public static final Function<String, byte[]> STRING_VALUE;

    static final MethodHandles.Lookup IMPL_LOOKUP;
    static final boolean OPEN_J9;
    static volatile MethodHandle CONSTRUCTOR_LOOKUP;
    static volatile boolean CONSTRUCTOR_LOOKUP_ERROR;
    static volatile Throwable initErrorLast;
    static volatile Throwable reflectErrorLast;
    static final AtomicInteger reflectErrorCount = new AtomicInteger();

    static {
        int jvmVersion = -1;
        boolean openj9 = false, android = false, graal = false;
        try {
            var jmvName = System.getProperty("java.vm.name");
            openj9 = jmvName.contains("OpenJ9");
            android = jmvName.equals("Dalvik");
            graal = jmvName.equals("Substrate VM");
            if (openj9 || android || graal) {
                FIELD_VALUE_STRING_ERROR = true;
            }

            var javaSpecVer = System.getProperty("java.specification.version");
            // android is 0.9
            if (javaSpecVer.startsWith("1.")) {
                javaSpecVer = javaSpecVer.substring(2);
            }
            if (javaSpecVer.indexOf('.') == -1) {
                jvmVersion = Integer.parseInt(javaSpecVer);
            }
        } catch (Throwable ignored) {
            initErrorLast = ignored;
        }

        OPEN_J9 = openj9;
        ANDROID = android;
        GRAAL = graal;

        boolean hasJavaSql = true;
        Class<?> dataSourceClass = null;
        Class<?> rowSetClass = null;
        try {
            dataSourceClass = Class.forName("javax.sql.DataSource");
            rowSetClass = Class.forName("javax.sql.RowSet");
        } catch (Throwable ignored) {
            hasJavaSql = false;
        }
        CLASS_SQL_DATASOURCE = dataSourceClass;
        CLASS_SQL_ROW_SET = rowSetClass;
        HAS_SQL = hasJavaSql;

        Class<?> transientClass = null;
        if (!android) {
            try {
                transientClass = Class.forName("java.beans.Transient");
            } catch (Throwable ignored) {
            }
        }
        CLASS_TRANSIENT = transientClass;

        JVM_VERSION = jvmVersion;

        if (JVM_VERSION == 8) {
            Field field = null;
            long fieldOffset = -1;
            try {
                field = String.class.getDeclaredField("value");
                field.trySetAccessible();
                fieldOffset = UnsafeUtils.objectFieldOffset(field);
            } catch (Exception ignored) {
                FIELD_VALUE_STRING_ERROR = true;
            }

            FIELD_STRING_VALUE = field;
            FIELD_STRING_VALUE_OFFSET = fieldOffset;
        } else {
            FIELD_VALUE_STRING_ERROR = true;
            FIELD_STRING_VALUE = null;
            FIELD_STRING_VALUE_OFFSET = -1;
        }

        boolean unsafeSupport;
        unsafeSupport = ((Predicate) o -> {
            try {
                return UnsafeUtils.UNSAFE != null;
            } catch (Throwable ignored) {
                return false;
            }
        }).test(null);
        UNSAFE_SUPPORT = unsafeSupport;

        BIG_ENDIAN = ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN;

        BiFunction<char[], Boolean, String> stringCreatorJDK8 = null;
        BiFunction<byte[], Byte, String> stringCreatorJDK11 = null;
        ToIntFunction<String> stringCoder = null;
        Function<String, byte[]> stringValue = null;

        MethodHandles.Lookup trustedLookup = null;

        {
            try {
                var lookupClass = MethodHandles.Lookup.class;
                // IMPL_LOOKUP 是用来判断私有方法是否被信任的标识，用来控制访问权限的.默认是false
                var implLookup = lookupClass.getDeclaredField("IMPL_LOOKUP");
                var fieldOffset = UnsafeUtils.UNSAFE.staticFieldOffset(implLookup);
                // implLookupField.trySetAccessible();
                // 当前这里只能通过Unsafe来获取，后续再试试其他的获取方式，比如被注释的通过反射获取的，既然有人这样写，理论上是可行的，只是某些条件不满足
                trustedLookup = (MethodHandles.Lookup) UnsafeUtils.UNSAFE.getObject(lookupClass, fieldOffset);
/*                implLookup =
                        (MethodHandles.Lookup) implLookupField.get(null); 这种方式获取不到，因为上面的trySetAccessible()会返回false表示设置失败，所以无法这样获取值
                        (MethodHandles.Lookup) unsafe.getObject(unsafe.staticFieldBase(implLookupField), unsafe.staticFieldOffset(implLookupField));*/
            } catch (Throwable ignored) {
                // ignored
            }
            if (trustedLookup == null) {
                trustedLookup = MethodHandles.lookup();
            }
            IMPL_LOOKUP = trustedLookup;
        }

        Boolean compact_strings = null;
        try {
            if (JVM_VERSION == 8 && trustedLookup != null) {
                var lookup = trustedLookup(String.class);

                var handle = lookup.findConstructor(
                        String.class, methodType(void.class, char[].class, boolean.class)
                );

                var callSite = LambdaMetafactory.metafactory(
                        lookup,
                        "apply",
                        methodType(BiFunction.class),
                        methodType(Object.class, Object.class, Object.class),
                        handle,
                        methodType(String.class, char[].class, boolean.class)
                );
                stringCreatorJDK8 = (BiFunction<char[], Boolean, String>) callSite.getTarget().invokeExact();
            }

            boolean lookupLambda = false;
            if (JVM_VERSION > 8 && trustedLookup != null && !android) {
                try {
                    var compact_strings_field = String.class.getDeclaredField("COMPACT_STRINGS");
                    if (compact_strings_field != null) {
                        if (UNSAFE_SUPPORT) {
                            var fieldOffset = UnsafeUtils.UNSAFE.staticFieldOffset(compact_strings_field);
                            compact_strings = UnsafeUtils.UNSAFE.getBoolean(String.class, fieldOffset);
                        } else {
                            compact_strings_field.trySetAccessible();
                            compact_strings = (Boolean) compact_strings_field.get(null);
                        }
                    }
                } catch (Throwable ignored) {
                    initErrorLast = ignored;
                }
                lookupLambda = compact_strings != null && compact_strings.booleanValue();
            }

            if (lookupLambda) {
                var lookup = trustedLookup.in(String.class);
                var handle = lookup.findConstructor(
                        String.class, methodType(void.class, byte[].class, byte.class)
                );
                var callSite = LambdaMetafactory.metafactory(
                        lookup,
                        "apply",
                        methodType(BiFunction.class),
                        methodType(Object.class, Object.class, Object.class),
                        handle,
                        methodType(String.class, byte[].class, Byte.class)
                );
                stringCreatorJDK11 = (BiFunction<byte[], Byte, String>) callSite.getTarget().invokeExact();

                var coder = lookup.findSpecial(
                        String.class,
                        "coder",
                        methodType(byte.class),
                        String.class
                );
                var applyAsInt = LambdaMetafactory.metafactory(
                        lookup,
                        "applyAsInt",
                        methodType(ToIntFunction.class),
                        methodType(int.class, Object.class),
                        coder,
                        methodType(byte.class, String.class)
                );
                stringCoder = (ToIntFunction<String>) applyAsInt.getTarget().invokeExact();

                var value = lookup.findSpecial(
                        String.class,
                        "value",
                        methodType(byte[].class),
                        String.class
                );
                var apply = LambdaMetafactory.metafactory(
                        lookup,
                        "apply",
                        methodType(Function.class),
                        methodType(Object.class, Object.class),
                        value,
                        methodType(byte[].class, String.class)
                );
                stringValue = (Function<String, byte[]>) apply.getTarget().invokeExact();
            }
        } catch (Throwable ignored) {
            initErrorLast = ignored;
        }

        if (stringCoder == null) {
            stringCoder = (str) -> 1;
        }

        STRING_CREATOR_JDK8 = stringCreatorJDK8;
        STRING_CREATOR_JDK11 = stringCreatorJDK11;
        STRING_CODER = stringCoder;
        STRING_VALUE = stringValue;
    }

    public static boolean isSQLDataSourceOrRowSet(Class<?> type) {
        return (CLASS_SQL_DATASOURCE != null && CLASS_SQL_DATASOURCE.isAssignableFrom(type))
                || (CLASS_SQL_ROW_SET != null && CLASS_SQL_ROW_SET.isAssignableFrom(type));
    }

    public static void setReflectErrorLast(Throwable error) {
        reflectErrorCount.incrementAndGet();
        reflectErrorLast = error;
    }

    public static char[] getCharArray(String str) {
        // GraalVM not support
        // Android not support
        if (!FIELD_VALUE_STRING_ERROR) {
            try {
                return (char[]) UnsafeUtils.UNSAFE.getObject(str, FIELD_STRING_VALUE_OFFSET);
            } catch (Exception ignored) {
                FIELD_VALUE_STRING_ERROR = true;
            }
        }

        return str.toCharArray();
    }

    /**
     * JDK 8开始支持Lambda，为了方便将一个Method映射为一个Lambda Function，避免反射开销。
     * java.invoke.LambdaMetafactory 可以实现这一功能，但这个也受限于可见性的限制，也就是说不能调用私有方法。
     * 有一个技巧，结合Unsafe，可以在不同版本的JDK都能构造一个Trusted MethodHandles.Lookup来绕开可见性的限制，调用任何JDK内部方法。
     *
     * @param objectClass
     * @return java.lang.invoke.MethodHandles.Lookup
     * @date 2022/12/29 15:49
     */
    public static MethodHandles.Lookup trustedLookup(Class<?> objectClass) {
        if (!CONSTRUCTOR_LOOKUP_ERROR) {
            try {
                var TRUSTED = -1;

                var constructor = CONSTRUCTOR_LOOKUP;
                if (JVM_VERSION < 15) {
                    if (constructor == null) {
                        constructor = IMPL_LOOKUP.findConstructor(
                                MethodHandles.Lookup.class,
                                methodType(void.class, Class.class, int.class)
                        );
                        CONSTRUCTOR_LOOKUP = constructor;
                    }
                    var FULL_ACCESS_MASK = 31; // for IBM Open J9 JDK
                    return (MethodHandles.Lookup) constructor.invoke(
                            objectClass,
                            OPEN_J9 ? FULL_ACCESS_MASK : TRUSTED
                    );
                } else {
                    if (constructor == null) {
                        constructor = IMPL_LOOKUP.findConstructor(
                                MethodHandles.Lookup.class,
                                methodType(void.class, Class.class, Class.class, int.class)
                        );
                        CONSTRUCTOR_LOOKUP = constructor;
                    }
                    return (MethodHandles.Lookup) constructor.invoke(objectClass, null, TRUSTED);
                }
            } catch (Throwable ignored) {
                CONSTRUCTOR_LOOKUP_ERROR = true;
            }
        }

        return IMPL_LOOKUP.in(objectClass);
    }

    // region access field
    public static void setField(Object obj, Object value, Field field) throws ReflectiveOperationException {
        if (obj == null) {
            setStaticField(field, value);
        } else {
            // 针对于非static的属性，即便是final的也可以通过下面的方式修改
            IMPL_LOOKUP.findVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).set(obj, value);
            // implLookup.findSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(obj, value);
            // unsafe.putObject(obj, unsafe.objectFieldOffset(field), value);
        }
    }

    public static void setStaticField(Field field, Object value) throws ReflectiveOperationException {
        try {
            IMPL_LOOKUP.ensureInitialized(field.getDeclaringClass());
            // implLookup.findStaticVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).set(value); // 这种支持static但不支持final的，更细节的可以看findStaticVarHandle()上的注释
            IMPL_LOOKUP.findStaticSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(value); // 这种可以，虽然注释似乎意思是不支持final的样子：if access checking fails, or if the field is not static or is final
            // Unsafe类的强大之处就在于，无视属性的访问限定，可以对其读取/修改。即便是static final的也一样
            // unsafe.putObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field), value);
        } catch (Throwable e) {
            throw new ReflectiveOperationException(e);
        }
    }

    public static <T> T getField(Object obj, Field field) throws ReflectiveOperationException {
        if (obj == null) {
            return getStaticField(field);
        } else {
            return (T) IMPL_LOOKUP.findVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).get(obj);
            // return (T) implLookup.findGetter(field.getDeclaringClass(),field.getName(),field.getType()).invoke(obj);
            // return (T) unsafe.getObject(obj, unsafe.objectFieldOffset(field));
        }
    }

    public static <T> T getStaticField(Field field) throws ReflectiveOperationException {
        IMPL_LOOKUP.ensureInitialized(field.getDeclaringClass());
        return (T) IMPL_LOOKUP.findStaticVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).get();
        // return (T) implLookup.findStaticGetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke();
        // return (T) unsafe.getObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field));

    }


    /**
     * 设置属性的值，可以设置 private static final的Field，后续看看都有哪些是Unsafe可以实现，但MethodHandle做不了的
     *
     * @param field  /
     * @param target /
     * @param value  /
     * @date 2022/5/1 12:10 AM
     */
    public static void setFailsafeFieldValue(Field field, Object target, Object value) throws Throwable {
        if (target != null) {
            IMPL_LOOKUP.findSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(target, value);
        } else {
            IMPL_LOOKUP.findStaticSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(value);
        }
    }
    // endregion
}
