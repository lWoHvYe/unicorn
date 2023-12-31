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

import com.lwohvye.core.exception.UtilsException;
import lombok.experimental.UtilityClass;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.ByteOrder;
import java.util.Arrays;
import java.util.function.Predicate;

import static java.lang.invoke.MethodType.methodType;

/**
 * <a href="https://github.com/alibaba/fastjson2/blob/main/core/src/main/java/com/alibaba/fastjson2/util/JDKUtils.java">...</a>
 */
@UtilityClass
@SuppressWarnings({"unchecked", "unused"})
public class JDKUtils {
    public static final int JVM_VERSION;

    static final Class<?> CLASS_SQL_DATASOURCE;
    static final Class<?> CLASS_SQL_ROW_SET;
    public static final boolean HAS_SQL;

    // Android not support
    public static final Class<?> CLASS_TRANSIENT;
    public static final boolean BIG_ENDIAN;

    public static final boolean UNSAFE_SUPPORT;

    static final MethodHandles.Lookup IMPL_LOOKUP;
    static final boolean OPEN_J9;
    static volatile MethodHandle CONSTRUCTOR_LOOKUP;
    static volatile boolean CONSTRUCTOR_LOOKUP_ERROR;
    static volatile Throwable initErrorLast;

    static {
        int jvmVersion = -1;
        boolean openj9 = false, android = false;
        try {
            var jmvName = System.getProperty("java.vm.name");
            openj9 = jmvName.contains("OpenJ9");
            android = jmvName.equals("Dalvik");

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

/*        var vector_support = false;
        try {
            if (JVM_VERSION >= 17) {
                // the following is a way to get inputArgs
                var factorClass = Class.forName("java.lang.management.ManagementFactory");
                var runtimeMXBeanClass = Class.forName("java.lang.management.RuntimeMXBean");
                var getRuntimeMXBean = factorClass.getMethod("getRuntimeMXBean");
                var runtimeMXBean = getRuntimeMXBean.invoke(null);
                var getInputArguments = runtimeMXBeanClass.getMethod("getInputArguments");
                var inputArguments = (List<String>) getInputArguments.invoke(runtimeMXBean);
//                vector_support = inputArguments.contains("--add-modules=jdk.incubator.vector");
                vector_support = true;
            }
        } catch (Throwable ignored) {
            initErrorLast = ignored;
        }
        VECTOR_SUPPORT = vector_support;*/

        boolean unsafeSupport;
        unsafeSupport = ((Predicate<Object>) o -> {
            try {
                return UnsafeUtils.UNSAFE != null;
            } catch (Throwable ignored) {
                return false;
            }
        }).test(null);
        UNSAFE_SUPPORT = unsafeSupport;

        BIG_ENDIAN = ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN;

        MethodHandles.Lookup trustedLookup = null;

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

    public static boolean isSQLDataSourceOrRowSet(Class<?> type) {
        return (CLASS_SQL_DATASOURCE != null && CLASS_SQL_DATASOURCE.isAssignableFrom(type))
                || (CLASS_SQL_ROW_SET != null && CLASS_SQL_ROW_SET.isAssignableFrom(type));
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

    /**
     * 属性拷贝，但使用其他lookup时存在导致♻️依赖的隐患(使用IMPL_LOOKUP后解决)，在这点上不如Field.set/get，
     * 且在 field.trySetAccessible()后，跳过了accessCheck，未必效率就低很多
     *
     * @param originalObj originalInstance
     * @param t           targetInstance
     * @param field       field to operate
     * @date 2023/2/22 9:16 AM
     */
    public static <T> void copyFieldVal(Object originalObj, T t, Field field) throws IllegalAccessException, NoSuchFieldException {
        var targetVarHandle = IMPL_LOOKUP.unreflectVarHandle(field);
        var originalVarHandle = IMPL_LOOKUP.findVarHandle(originalObj.getClass(), field.getName(), field.getType());
        targetVarHandle.set(t, originalVarHandle.get(originalObj));
    }

    /**
     * 只支持简单无参调用，invoke
     *
     * @date 2023/2/22 1:21 PM
     */
    public static <T> T invokeMethod(Object obj, String methodName, Class<?> rType) {
        var mt = MethodType.methodType(rType);
        try {
            var methodHandle = IMPL_LOOKUP.findVirtual(obj.getClass(), methodName, mt);
            var result = methodHandle.invoke(obj);
            return (T) result;
        } catch (Throwable e) {
            throw new UtilsException("invoke error: " + e.getMessage());
        }
    }

    /**
     * MethodType的pTypes需要与目标方法签名一致（不支持向上/向下转型，比如用Integer去获取入参为Number的方法是不允许的，同样用Number去获取入参为Integer的方法也不行。这个还能理解），
     * rtype需要与目标方法的返回值一致（不允许向上/向下转型。不允许向下很容易理解，为何要不允许向上转型，有点不理解）。以上并不是下面问题的原因
     * 该方法的使用场景很有限，因为虽然invoke的入参是可变参数，但本方法入参中的可变参数会被转成数组，把数组传给invoke与本方法的原入参是不一样的 (原来会是包含各种类型的可变参数，转换后变成单纯的Object[])，也就导致了与直接调用存在差异
     * 这里出问题，感觉是因为处理时，底层把Object[]看成一个独立的参数了，而非当可变参数处理，可以这样理解，转成数组是结果，直接传数组时，普通方法间能应对，但MethodHandle不行，这又牵涉到其底层机制了，有时间可以再看看
     *
     * @param obj        callInstance
     * @param methodName invokeMethodName
     * @param rType      returnType
     * @param params     args
     * @return T
     * @date 2023/2/22 11:00 AM
     */
    @Deprecated(forRemoval = true)
    public static <T> T invokeMethod(Object obj, String methodName, Class<?> rType, Object... params) {
        var pTypes = Arrays.stream(params).map(Object::getClass).toArray(Class<?>[]::new);
        var mt = MethodType.methodType(rType, pTypes);
        try {
            var methodHandle = IMPL_LOOKUP.findVirtual(obj.getClass(), methodName, mt);
            var result = params.length != 0 ? methodHandle.invoke(obj, params) : methodHandle.invoke(obj);
            return (T) result;
        } catch (Throwable e) {
            throw new UtilsException("invoke error: " + e.getMessage());
        }
    }

    /**
     * get method through reflect，可以用来获取private的方法，然后做UT，
     * 但注意在invoke时，若只有一个参数，且此次值为null，则不能直接传null，而应是 new Object[] { null }, 其他时候都可以用可变参数的形式invoke
     *
     * @param targetClass    /
     * @param methodName     /
     * @param parameterTypes Signature-NotNull
     * @return java.lang.reflect.Method
     */
    public static Method getAccessibleMethod(@NotNull Class<?> targetClass, @NotNull String methodName, @Nullable Class<?>... parameterTypes) throws NoSuchMethodException {
        var method = targetClass.getDeclaredMethod(methodName, parameterTypes);
        method.trySetAccessible();
        return method;
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

    // region optional
    // check if the give Jar is added as dependency
    public static Boolean checkFuncEnable(String clazzName) {
        try {
            Class.forName(clazzName);
        } catch (ReflectiveOperationException ignored) {
            return false;
        }
        return true;
    }

    // endregion
}
