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

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodType;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.lwohvye.core.utils.JDKUtils.IMPL_LOOKUP;
import static com.lwohvye.core.utils.JDKUtils.setFailsafeFieldValue;

/**
 * 动态修改枚举类。也包含了对static final Field的修改方法
 * After Java 9, sun.reflect package was moved to jdk.internal.reflect and it requires extra operations to access.
 * After Java 12, all members in java.lang.reflect.Field class were added to jdk.internal.reflect.Reflection#fieldFilterMap so that it was unable to access by using reflection.
 * So the most reasonable way is to use java.lang.invoke.MethodHandles$Lookup#IMPL_LOOKUP to access each member after Java 8.
 * See: <a href="https://stackoverflow.com/questions/61141836/change-static-final-field-in-java-12">...</a>
 * How to rewrite a static final field in jdk12+
 *
 * @date 2022/4/30 4:50 PM
 */
@UtilityClass
public class DynamicEnumHelper {

    private static <T extends Enum<?>> T makeEnum(Class<T> enumClass, String value, int ordinal, Class<?>[] additionalTypes, Object[] additionalValues) throws Throwable {
        var additionalParamsCount = additionalValues == null ? 0 : additionalValues.length;
        var params = new Object[additionalParamsCount + 2];
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
        return IMPL_LOOKUP.findConstructor(enumClass, MethodType.methodType(void.class, parameterTypes));
    }

    private static void cleanEnumCache(Class<?> enumClass) throws Throwable {
        blankField(enumClass, "enumConstantDirectory");
        blankField(enumClass, "enumConstants");
        blankField(enumClass, "enumVars");
    }

    private static void blankField(Class<?> enumClass, String fieldName) throws Throwable {
        for (var field : Class.class.getDeclaredFields()) {
            if (field.getName().contains(fieldName)) {
                setFailsafeFieldValue(field, enumClass, null);
                break;
            }
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
        if (!Enum.class.isAssignableFrom(enumType)) {
            throw new RuntimeException("class " + enumType + " is not an instance of Enum");
        }

        // 1. Lookup "$VALUES" holder in enum class and get previous enum instances
        Field valuesField = null;
        var fields = enumType.getDeclaredFields();

        for (var field : fields) {
            var name = field.getName();
            if (name.equals("$VALUES") || name.equals("ENUM$VALUES")) //Added 'ENUM$VALUES' because Eclipse's internal compiler doesn't follow standards
            {
                valuesField = field;
                break;
            }
        }

        int flags = (Modifier.PUBLIC) | Modifier.STATIC | Modifier.FINAL | 0x1000 /*SYNTHETIC*/;
        if (valuesField == null) {
            var valueType = String.format("[L%s;", enumType.getName().replace('.', '/'));

            for (var field : fields) {
                if ((field.getModifiers() & flags) == flags && // 最内层的括号通过与运算做可见性判断
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
            for (var field : fields) {
                var mods = String.format("%16s", Integer.toBinaryString(field.getModifiers())).replace(' ', '0');
                lines.add(String.format("       %s %s: %s", mods, field.getName(), field.getType().getName()));
            }

            return null;
        }

        valuesField.trySetAccessible();

        try {
            // 2. Copy it
            var previousValues = (T[]) valuesField.get(enumType);
            var values = new ArrayList<>(Arrays.asList(previousValues));

            // 3. build new enum
            // 枚举本身是有一个String类型的value和int类型的ordinal，所以在下面的逻辑中会加两个参数进去
            var newValue = makeEnum(enumType, enumName, values.size(), paramTypes, paramValues);

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


    // public static void main(String[] args) throws ReflectiveOperationException {
    //     System.out.println(Arrays.toString(CodeBiEnum.values()));
    //     addEnum(CodeBiEnum.class, "FOUR", new Class[]{Integer.class, String.class}, new Object[]{4, "动态加进去一个"});
    //     System.out.println(Arrays.toString(CodeBiEnum.values()));
    //     var des = CodeBiEnum.class.getDeclaredField("des");
    //     setField(null, "sds", des);
    //     var code = CodeBiEnum.class.getDeclaredField("code");
    //     setField(CodeBiEnum.ONE, 5, code);
    // }

    /**
     * 在Java 12之前，可以通过该方式，修改final Field，针对static final Field也可以。已验证
     *
     * @param object        /
     * @param fieldName     /
     * @param newFieldValue /
     * @date 2022/5/2 9:06 AM
     * @deprecated Java 12 is unsupported for this way
     */
    @Deprecated(since = "Java 12")
    public static void modifyFinalField(Object object, String fieldName, Object newFieldValue) throws NoSuchFieldException, IllegalAccessException {
        var field = object.getClass().getDeclaredField(fieldName);
        var modifiersField = Field.class.getDeclaredField("modifiers"); // 获取Field的访问修饰符，java.lang.NoSuchFieldException: modifiers
        modifiersField.trySetAccessible(); //Field 的 modifiers 是私有的
        modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL); // 将Field的访问修饰符设置为非final的
        field.trySetAccessible();
        field.set(object, newFieldValue);
        // 另，field本身是一个副本，我们修改的Modifier和Accessible都是针对该副本的，若在此处再次获取个fieldNew，其Modifier之类的还是原来的。这一点需记住
        // 但有的框架中对Reflect做了Cache以提高效率，这样以来Reflect可能拿到的就是之前的副本了，所以accessible最好在使用完后重制一下
        // var fieldAfterModifier = object.getClass().getDeclaredField(fieldName); // 如果原来是final的话，这里还是final的
    }

}
