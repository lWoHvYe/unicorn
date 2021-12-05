/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

/**
 * @description 具体较复杂，这里只做留档，标识可以通过一些方法获取到T.class
 * <a href="https://www.lwohvye.com/2021/12/05/java%e4%b8%ad%e7%9a%84class%e7%b1%bb/">Class.java</a>
 * @date 2021/11/13 5:49 下午
 */
public class GenericsUtils {
    /**
     * @param clazz The class to introspect
     * @return the first generic declaration, or <code>Object.class</code> if cannot be determined
     * @description 通过反射, 获得定义Class时声明的父类的范型参数的类型.
     * 如public BookManager extends GenricManager<Book>
     */
    public static Class getSuperClassGenricType(Class clazz) {
        return getSuperClassGenricType(clazz, 0);
    }

    /**
     * @param clazz clazz The class to introspect
     * @param index the Index of the generic declaration,start from 0.
     * @description 通过反射, 获得定义Class时声明的父类的范型参数的类型.需注意拿的是父类的
     * 如public BookManager extends GenricManager<Book>
     */
    public static Class getSuperClassGenricType(Class clazz, int index) throws IndexOutOfBoundsException {
        // 返回表示此 Class 所表示的实体（类、接口、基本类型或 void）的直接超类的 Type。
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

    // 只能获取超类上的泛型信息，使用场景十分的有限
    public static void main(String[] args) {
        ArrayList<String> arr = new ArrayList<>();
        var superClassGenricType = getSuperClassGenricType(arr.getClass()); // 无法获取
        var arr1 = new ArrayList<String>();
        var superClassGenricType1 = getSuperClassGenricType(arr1.getClass()); // 无法获取
        List<String> arr2 = new ArrayList<>();
        var superClassGenricType2 = getSuperClassGenricType(arr2.getClass()); // 无法获取
        var arr3 = new MyList();
        var superClassGenricType3 = getSuperClassGenricType(arr3.getClass()); // 可以获取
    }
}

class MyList extends ArrayList<String> {

}
