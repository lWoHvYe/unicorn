/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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

import org.jetbrains.annotations.NotNull;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 具体较复杂，这里只做留档，标识可以通过一些方法获取到T.class
 * <a href="https://www.lwohvye.com/2021/12/05/java%e4%b8%ad%e7%9a%84class%e7%b1%bb/">Class.java</a>
 *
 * @date 2021/11/13 5:49 下午
 */
public class GenericsUtils {
    /**
     * 通过反射, 获得定义Class时声明的父类的范型参数的类型.
     * 如public BookManager extends GenricManager<Book>
     *
     * @param clazz The class to introspect
     * @return the first generic declaration, or <code>Object.class</code> if cannot be determined
     */
    public static Class getSuperClassGenricType(Class clazz) {
        return getSuperClassGenricType(clazz, 0);
    }

    /**
     * 通过反射, 获得定义Class时声明的父类的范型参数的类型.需注意拿的是父类的
     * 如public BookManager extends GenricManager<Book>
     *
     * @param clazz clazz The class to introspect
     * @param index the Index of the generic declaration,start from 0.
     */
    public static Class getSuperClassGenricType(Class clazz, int index) throws IndexOutOfBoundsException {
        // 返回表示此 Class 所表示的实体（类、接口、基本类型或 void）的直接超类的 Type。
        // getGenericSuperclass(): Returns the Type representing the direct superclass of the entity (class, interface, primitive type or void) represented by this Class object.
        Type genType = clazz.getGenericSuperclass();

        if (genType instanceof ParameterizedType ptzType) {
            var actualTypeArguments = ptzType.getActualTypeArguments(); // 该超类的泛型数组
            var rawType = ptzType.getRawType(); // 该超类的原始类型（不带类型信息）
            var ownerType = ptzType.getOwnerType(); // 该超类为内部类时，O.I时，返回O。这部分未验证
            if (index >= 0 && index < actualTypeArguments.length) {
                var actualTypeArgument = actualTypeArguments[index];
                return actualTypeArgument instanceof Class<?> actualType ? actualType : Object.class;
            }
        }
        return Object.class;
    }

    public static void printClass(Class<?> cl) {
        System.out.print(cl);
        // getTypeParameters获取泛型类型变量
        printTypes(cl.getTypeParameters(), "<", ", ", ">", true);
        Type sc = cl.getGenericSuperclass();
        if (sc != null) {
            System.out.print(" extends ");
            printType(sc, false);
        }
        // getGenericInterfaces(): Returns the Types representing the interfaces directly implemented by the class or interface represented by this Class object.
        printTypes(cl.getGenericInterfaces(), " implements ", ", ", "", false);
        System.out.println();
    }

    public static void printMethod(Method m) {
        String name = m.getName();
        System.out.print(Modifier.toString(m.getModifiers()));
        System.out.print(" ");
        printTypes(m.getTypeParameters(), "<", ", ", ">", true);

        printType(m.getGenericReturnType(), false);
        System.out.print(" ");
        System.out.print(name);
        System.out.print(" (");
        //getGenericParameterTypes  获得这个方法被声明的泛型参数类型
        printTypes(m.getGenericParameterTypes(), "", ", ", "", false);
        System.out.println(")");
    }

    public static void printTypes(Type[] types, String pre, String sep, String suf, boolean isDefinition) {
        if (pre.equals(" extends ") && Arrays.equals(types, new Type[]{Object.class})) return;
        if (types.length > 0) System.out.print(pre);
        for (int i = 0; i < types.length; i++) {
            if (i > 0) System.out.print(sep);
            printType(types[i], isDefinition);
        }
        if (types.length > 0) System.out.print(suf);
    }

    public static void printType(Type type, boolean isDefinition) {
        if (type instanceof Class t) { // 描述具体类型
            System.out.print(t.getName());
        } else if (type instanceof TypeVariable t) { // 描述类型变量(如 T extends Comparable<? super T> )
            System.out.print(t.getName());
            if (isDefinition)
                printTypes(t.getBounds(), " extends ", " & ", "", false);
        } else if (type instanceof WildcardType t) { // 描述通配符 (如?super T )
            System.out.print("?");
            printTypes(t.getUpperBounds(), " extends ", " & ", "", false);
            printTypes(t.getLowerBounds(), " super ", " & ", "", false);
        } else if (type instanceof ParameterizedType t) { // 描述泛型类或接口类型(如 Comparable<? super T> 。)
            Type owner = t.getOwnerType();
            if (owner != null) {
                printType(owner, false);
                System.out.print(".");
            }
            printType(t.getRawType(), false);
            printTypes(t.getActualTypeArguments(), "<", ", ", ">", false);
        } else if (type instanceof GenericArrayType t) { // 描述泛型数组(如 T[ ]。)
            System.out.print("");
            printType(t.getGenericComponentType(), isDefinition);
            System.out.print("[]");
        }
    }

    public static <T> T[] convertList2Array(List<T> list, Class<T> componentType) {
        return list.toArray((T[]) Array.newInstance(componentType, list.size()));
    }

    // 只能获取超类上的泛型信息，使用场景十分的有限
    public static void main(String[] args) {
        ArrayList<String> arr = new ArrayList<>();
        var superClassGenricType = getSuperClassGenricType(arr.getClass()); // 无法获取
        printClass(arr.getClass()); // class java.util.ArrayList<E> extends java.util.AbstractList<E> implements java.util.List<E>, java.util.RandomAccess, java.lang.Cloneable, java.io.Serializable
        var arr1 = new ArrayList<String>();
        var superClassGenricType1 = getSuperClassGenricType(arr1.getClass()); // 无法获取
        printClass(arr1.getClass()); // class java.util.ArrayList<E> extends java.util.AbstractList<E> implements java.util.List<E>, java.util.RandomAccess, java.lang.Cloneable, java.io.Serializable
        List<String> arr2 = new ArrayList<>();
        var superClassGenricType2 = getSuperClassGenricType(arr2.getClass()); // 无法获取
        printClass(arr2.getClass()); // class java.util.ArrayList<E> extends java.util.AbstractList<E> implements java.util.List<E>, java.util.RandomAccess, java.lang.Cloneable, java.io.Serializable
        var arr3 = new MyList<>(3L);
        var superClassGenricType3 = getSuperClassGenricType(arr3.getClass()); // 可以获取
        var t3 = arr3.getT(); // 编译时做了替换，t3的类型是能拿到的
        printClass(arr3.getClass()); // 这里是拿不到T的类型的 // class com.lwohvye.core.utils.MyList<T> extends java.util.ArrayList<java.lang.String> implements java.lang.Comparable<java.lang.Integer>
        var arr4 = new MyList<>(true);
        var superClassGenricType4 = getSuperClassGenricType(arr4.getClass()); // 可以获取
        var t4 = arr4.getT();
        printClass(arr4.getClass()); // class com.lwohvye.core.utils.MyList<T> extends java.util.ArrayList<java.lang.String> implements java.lang.Comparable<java.lang.Integer>
    }
}

class MyList<T> extends ArrayList<String> implements Comparable<Integer> {

    private T t;

    MyList(T t) {
        this.t = t;
    }

    public T getT() {
        return t;
    }

    @Override
    public int compareTo(@NotNull Integer o) {
        return 0;
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
