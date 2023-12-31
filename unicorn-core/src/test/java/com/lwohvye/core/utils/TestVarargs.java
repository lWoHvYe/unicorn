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

package com.lwohvye.core.utils;

import org.junit.jupiter.api.Test;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public class TestVarargs {


    @Test
    void test1() throws Throwable {
        var varArgs = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs", MethodType.methodType(void.class, String.class, Boolean.class));
        varArgs.invoke(this, "str", true); // pass
        System.out.println("---------");
        var varArgs2 = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs", MethodType.methodType(void.class, new Class[]{String.class, Boolean.class}));
        varArgs2.invoke(this, "str", true); // pass
    }

    @Test
    void test2() throws Throwable {
        // java.lang.invoke.WrongMethodTypeException: cannot convert MethodHandle(TestVarargs,String,Boolean)void to (TestVarargs,Object[])void
        var varArgs = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs", MethodType.methodType(void.class, String.class, Boolean.class));
        varArgs.invoke(this, new Object[]{"str", true}); // failed
        System.out.println("---------");
        var varArgs2 = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs", MethodType.methodType(void.class, new Class[]{String.class, Boolean.class}));
        varArgs2.invoke(this, new Object[]{"str", true}); // failed
    }

    void varArgs(String s, Boolean b) {
        System.out.println(s + " | " + b);
    }


    @Test
    void test4() throws Throwable {
        varArgs2("str", false);
    }

    void varArgs2(Object... args) throws Throwable {
        // java.lang.invoke.WrongMethodTypeException: cannot convert MethodHandle(TestVarargs,String,Boolean)void to (TestVarargs,Object[])void
        var varArgs = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs", MethodType.methodType(void.class, String.class, Boolean.class));
        varArgs.invoke(this, args); // failed
        System.out.println("---------");
        var varArgs2 = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs", MethodType.methodType(void.class, new Class[]{String.class, Boolean.class}));
        varArgs2.invoke(this, args); // failed
    }

    @Test
    void test3() throws Throwable {
        varArgs4("str");
    }

    void varArgs4(Object... args) throws Throwable {
        // java.lang.ClassCastException: Cannot cast [Ljava.lang.Object; to java.lang.String  这里注意是Object[]，一个参数时稍不注意就被掩盖了重要的问题
        var varArgs = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs1", MethodType.methodType(void.class, String.class));
        varArgs.invoke(this, args); // failed
        System.out.println("---------");
        var varArgs2 = MethodHandles.lookup().findVirtual(TestVarargs.class, "varArgs1", MethodType.methodType(void.class, new Class[]{String.class}));
        varArgs2.invoke(this, args); // failed
    }

    void varArgs1(String str) {
        System.out.println(str);
    }

    @Test
    void test5() { // pass
        varArgs3("a", "b");
    }

    void varArgs3(String... args) {
        varArgs6(args);
        System.out.println(args);
    }

    void varArgs6(String... args) {
        System.out.println(args);
    }
}
