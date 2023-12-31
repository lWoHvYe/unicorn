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

import com.lwohvye.core.utils.ExceptionMsgUtils;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class AnyExtensionMethod {
    /**
     * 构建一个Comparable Type 的 fieldType，跟fieldType一样或者为null（这里返回null是因为如果不是Comparable，那些比较类的Query是无法invoke的）
     * 这个参数只是为了解决几个警告，因为fieldType不一定extends Comparable，所以加了这个，来限定需要能够比较才行。
     * 因为cb.lessThan,greaterThan,between的返回值 <Y extends Comparable<? super Y>>， 入参 （Expression<? extends Y> var1, Y var2, Y var3），含义:类型 Y 必须实现 Comparable 接口，并且这个接口的类型是 Y 或 Y 的任一父类
     * https://www.lwohvye.com/2021/12/04/t%e3%80%81-super-t%e3%80%81-extends-t/
     *
     * @param value /
     * @return java.lang.Class
     * @date 2022/9/17 5:21 PM
     */
    @Nullable
    // private static <C> Class<? extends Comparable<? super C>> castComparableFieldType(Object val) 因为C 未指定，所以实际上是Object，? super Object只能是Object，所以干脆就用Object了
    // 2022/9/17 总感觉这里是不对的，`? extends Comparable<Object>` 和 `? extends Comparable<? super Y>` 的含义是不一样的，
    //  Any Class implements Comparable<Object> 与 Any Class implements Comparable & the type of this interface is Y or Y's parent，but 这当前业务中，Y是没办法被define的
    //  明明应该用后者，但前者这种编译能通过，运行也不报错，但总感觉哪里不对，泛型还需进一步的学习(显然跟泛型擦除有关, 因为采用了类型擦除，泛型类型只在静态类型检查时期存在，在这之后，程序中所有的泛型类型都会被擦除，并替换为它们的非泛型上界)
    //  疑问是 String implements Comparable<String>, Timestamp extends Date (Date implements Comparable<Date>) 与 XXX implements Comparable<Object> 区别与相似之处
    // 2022/9/18 结合字节码，Comparable<T> 中的 compareTo方法，依旧是compareTo(Object obj)，这样上面的疑问便解决了:他们实际上是一样的，只是内部有的有转型
    /*
    // class version 62.0 (62)
    // access flags 0x601
    // signature <T:Ljava/lang/Object;>Ljava/lang/Object;
    // declaration: java/lang/Comparable<T>
    public abstract interface java/lang/Comparable {

    // compiled from: Comparable.java

    // access flags 0x401
    // signature (TT;)I
    // declaration: int compareTo(T)
    public abstract compareTo(Ljava/lang/Object;)I
    }
     */
    @SuppressWarnings("unchecked")
    public static Class<? extends Comparable<Object>> castComparableFieldType(Object value) {
        return value instanceof Comparable<?> cec ? (Class<? extends Comparable<Object>>) cec.getClass() : null;
    }

    /* 在此记录几种行不通的方案，以防后续忘记再次做重复的尝试
    private static <Y extends Comparable<? super Y>> Class<? extends Y> castComparableFieldType(Object val) {} // 基于调用的方式，build时可能报错，并且这个Y感觉没办法define
    private static <Y extends Comparable<? super Y>> Class<? extends Y> castComparableFieldType(Object val, Class<Y> yClass) {} // 当前业务中yClass是没法define的，且还有泛型上的限制，最重要的，如果yClass明了，这方法没调用的必要了
    private static <Y> Class<Y extends Comparable<? super Y>> castComparableFieldType(Object val) {} // 直接语法错误 Unexpected bound
    */

    public static Comparable<Object> castToComparable(Object value, String attributeName, Class<? extends Comparable<Object>> tClass) {
        return Objects.requireNonNull(tClass, ExceptionMsgUtils.genUnComparableExcMsg(attributeName)).cast(value);
    }
}
