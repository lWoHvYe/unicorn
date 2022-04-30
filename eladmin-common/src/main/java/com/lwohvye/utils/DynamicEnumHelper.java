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

package com.lwohvye.utils;


import com.lwohvye.utils.enums.CodeBiEnum;
import sun.misc.Unsafe;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 动态修改枚举类。也包含了对static final Field的修改方法
 *
 * @date 2022/4/30 4:50 PM
 */
public class DynamicEnumHelper {
    private static MethodHandles.Lookup implLookup = null;
    private static boolean isSetup = false;
    private static Unsafe unsafe = null;

    private static void setup() {
        if (isSetup) {
            return;
        }

        try {
            /*
             * After Java 9, sun.reflect package was moved to jdk.internal.reflect and it requires extra operations to access.
             * After Java 12, all members in java.lang.reflect.Field class were added to jdk.internal.reflect.Reflection#fieldFilterMap so that it was unable to access by using reflection.
             * So the most reasonable way is to use java.lang.invoke.MethodHandles$Lookup#IMPL_LOOKUP to access each member after Java 8.
             * See: https://stackoverflow.com/questions/61141836/change-static-final-field-in-java-12
             * How to rewrite a static final field in jdk12+
             */
            // 这里只能这样获取，通过Unsafe.getUnsafe()直接获取会抛出异常
            Class<?> unsafeClass = Class.forName("sun.misc.Unsafe");
            Field unsafeField = unsafeClass.getDeclaredField("theUnsafe");
            unsafeField.trySetAccessible();
            unsafe = (Unsafe) unsafeField.get(null);
            /*
            Field IMPL_LOOKUP = MethodHandles.Lookup.class.getDeclaredField("IMPL_LOOKUP");
            IMPL_LOOKUP.setAccessible(true);
            MethodHandles.Lookup lkp = (MethodHandles.Lookup) IMPL_LOOKUP.get(null);
            MethodHandle methodHandle= lkp.findSpecial(t.getClass(), getMethodName, MethodType.methodType(String.class), t.getClass());
            value = methodHandle.bindTo(t).invoke();
            */
            // IMPL_LOOKUP 是用来判断私有方法是否被信任的标识，用来控制访问权限的.默认是false
            Field implLookupField = MethodHandles.Lookup.class.getDeclaredField("IMPL_LOOKUP");
            // implLookupField.trySetAccessible();
            implLookup =
                    // (MethodHandles.Lookup) implLookupField.get(null); 这种方式获取不到。。。。
                    (MethodHandles.Lookup) unsafe.getObject(unsafe.staticFieldBase(implLookupField), unsafe.staticFieldOffset(implLookupField));
        } catch (Exception ignored) {
        }

        isSetup = true;
    }

    private static <T extends Enum<?>> T makeEnum(Class<T> enumClass, String value, int ordinal, Class<?>[] additionalTypes, Object[] additionalValues) throws Throwable {
        int additionalParamsCount = additionalValues == null ? 0 : additionalValues.length;
        Object[] params = new Object[additionalParamsCount + 2];
        params[0] = value;
        params[1] = ordinal;
        if (additionalValues != null) {
            System.arraycopy(additionalValues, 0, params, 2, additionalValues.length);
        }
        return enumClass.cast(getConstructorAccessor(enumClass, additionalTypes).invokeWithArguments(params));
    }

    /*
     * Everything below this is found at the site below, and updated to be able to compile in Eclipse/Java 1.6+
     * Also modified for use in decompiled code.
     * Found at: http://niceideas.ch/roller2/badtrash/entry/java_create_enum_instances_dynamically
     */
    private static MethodHandle getConstructorAccessor(Class<?> enumClass, Class<?>[] additionalParameterTypes) throws Exception {
        Class<?>[] parameterTypes = new Class[additionalParameterTypes.length + 2];
        parameterTypes[0] = String.class;
        parameterTypes[1] = int.class;
        System.arraycopy(additionalParameterTypes, 0, parameterTypes, 2, additionalParameterTypes.length);
        return implLookup.findConstructor(enumClass, MethodType.methodType(void.class, parameterTypes));
    }

    private static void cleanEnumCache(Class<?> enumClass) throws Throwable {
        blankField(enumClass, "enumConstantDirectory");
        blankField(enumClass, "enumConstants");
        blankField(enumClass, "enumVars");
    }

    private static void blankField(Class<?> enumClass, String fieldName) throws Throwable {
        for (Field field : Class.class.getDeclaredFields()) {
            if (field.getName().contains(fieldName)) {
                setFailsafeFieldValue(field, enumClass, null);
                break;
            }
        }
    }

    public static void setFailsafeFieldValue(Field field, Object target, Object value) throws Throwable {
        if (target != null) {
            implLookup.findSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(target, value);
        } else {
            implLookup.findStaticSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(value);
        }
    }

    public static <T extends Enum<?>> T addEnum0(Class<T> enumType, String enumName, Class<?>[] paramTypes, Object... paramValues) {
        return addEnum(enumType, enumName, paramTypes, paramValues);
    }

    /**
     * Add an enum instance to the enum class given as argument
     *
     * @param <T>      the type of the enum (implicit)
     * @param enumType the class of the enum to be modified
     * @param enumName the name of the new enum instance to be added to the class.
     */
    @SuppressWarnings({"unchecked"})
    public static <T extends Enum<?>> T addEnum(final Class<T> enumType, String enumName, final Class<?>[] paramTypes, Object[] paramValues) {
        // 0. Sanity checks
        if (!isSetup) {
            setup();
        }

        // 1. Lookup "$VALUES" holder in enum class and get previous enum instances
        Field valuesField = null;
        Field[] fields = enumType.getDeclaredFields();

        for (Field field : fields) {
            String name = field.getName();
            if (name.equals("$VALUES") || name.equals("ENUM$VALUES")) //Added 'ENUM$VALUES' because Eclipse's internal compiler doesn't follow standards
            {
                valuesField = field;
                break;
            }
        }

        int flags = (Modifier.PUBLIC) | Modifier.STATIC | Modifier.FINAL | 0x1000 /*SYNTHETIC*/;
        if (valuesField == null) {
            String valueType = String.format("[L%s;", enumType.getName().replace('.', '/'));

            for (Field field : fields) {
                if ((field.getModifiers() & flags) == flags &&
                    field.getType().getName().replace('.', '/').equals(valueType)) //Apparently some JVMs return .'s and some don't..
                {
                    valuesField = field;
                    break;
                }
            }
        }

        if (valuesField == null) {
            final List<String> lines = new ArrayList<>();
            lines.add(String.format("Could not find $VALUES field for enum: %s", enumType.getName()));
            //lines.add(String.format("Runtime Deobf: %s", FMLForgePlugin.RUNTIME_DEOBF));
            lines.add(String.format("Flags: %s", String.format("%16s", Integer.toBinaryString(flags)).replace(' ', '0')));
            lines.add("Fields:");
            for (Field field : fields) {
                String mods = String.format("%16s", Integer.toBinaryString(field.getModifiers())).replace(' ', '0');
                lines.add(String.format("       %s %s: %s", mods, field.getName(), field.getType().getName()));
            }

            return null;
        }

        valuesField.trySetAccessible();

        try {
            // 2. Copy it
            T[] previousValues = (T[]) valuesField.get(enumType);
            List<T> values = new ArrayList<>(Arrays.asList(previousValues));

            // 3. build new enum
            T newValue = (T) makeEnum(enumType, enumName, values.size(), paramTypes, paramValues);

            // 4. add new value
            values.add(newValue);

            // 5. Set new values field
            setFailsafeFieldValue(valuesField, null, values.toArray((T[]) Array.newInstance(enumType, 0)));

            // 6. Clean enum cache
            cleanEnumCache(enumType);

            return newValue;
        } catch (Throwable throwable) {
            throwable.printStackTrace();
            throw new RuntimeException(throwable.getMessage(), throwable);
        }
    }

    static {
        if (!isSetup) {
            setup();
        }
    }

    public static void setField(Object obj, Object value, Field field) throws ReflectiveOperationException {
        if (obj == null) {
            setStaticField(field, value);
        } else {
            try {
                unsafe.putObject(obj, unsafe.objectFieldOffset(field), value);
            } catch (Exception e) {
                throw new ReflectiveOperationException(e);
            }
        }
    }

    public static void setStaticField(Field field, Object value) throws ReflectiveOperationException {
        try {
            implLookup.ensureInitialized(field.getDeclaringClass());
            unsafe.putObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field), value);
        } catch (Exception e) {
            throw new ReflectiveOperationException(e);
        }
    }

    public static <T> T getField(Object obj, Field field) throws ReflectiveOperationException {
        if (obj == null) {
            return getStaticField(field);
        } else {
            try {
                return (T) unsafe.getObject(obj, unsafe.objectFieldOffset(field));
            } catch (Exception e) {
                throw new ReflectiveOperationException(e);
            }
        }
    }

    public static <T> T getStaticField(Field field) throws ReflectiveOperationException {
        try {
            implLookup.ensureInitialized(field.getDeclaringClass());
            return (T) unsafe.getObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field));
        } catch (Exception e) {
            throw new ReflectiveOperationException(e);
        }
    }

    public static void main(String[] args) {
        System.out.println(Arrays.toString(CodeBiEnum.values()));
        addEnum(CodeBiEnum.class, "FOUR", new Class[]{Integer.class, String.class}, new Object[]{4, "动态加进去一个"});
        System.out.println(Arrays.toString(CodeBiEnum.values()));
    }
}
