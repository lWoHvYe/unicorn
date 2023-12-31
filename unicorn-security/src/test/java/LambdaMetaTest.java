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

import org.junit.jupiter.api.Test;

import java.lang.invoke.LambdaMetafactory;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.function.Function;

class LambdaMetaTest {

    @Test
    void testLambdaParams() throws Throwable {

        MultiFunction fun = Function::apply;
        var i = fun.runFun(Integer::valueOf, "2");

        var lookup = MethodHandles.lookup();
        var runFun = lookup.findVirtual(MultiFunction.class, "runFun", MethodType.methodType(Integer.class, Function.class, String.class));
//        runFun.invoke(fun, Integer::valueOf, "3"); // 编译error: Object is not a functional interface
        Function<String, Integer> innerFun = Integer::valueOf; // 虽然直接不行，但可以封装成Function，然后就可以了
        var i2 = runFun.invoke(fun, innerFun, "3"); // 所以findVirtual也可以应对参数包含lambda的method

        var multiFunction = (MultiFunction) LambdaMetafactory.metafactory(
                lookup,
                "runFun", // interfaceMethodName
                MethodType.methodType(MultiFunction.class), // factoryType
                MethodType.methodType(Integer.class, Function.class, String.class), // interfaceMethodType
                lookup.findVirtual(Function.class, "apply", MethodType.methodType(Object.class, Object.class)), // implementation
                MethodType.methodType(Integer.class, Function.class, String.class) // dynamicMethodType
        ).getTarget().invokeExact();
        var i4 = multiFunction.runFun(Integer::valueOf, "4");

    }


    @Test
    void testInvoke() throws Throwable { // 复用前291ms，函数复用后54ms
        MultiFunction fun = Function::apply;
        var lookup = MethodHandles.lookup();
        var runFun = lookup.findVirtual(MultiFunction.class, "runFun", MethodType.methodType(Integer.class, Function.class, String.class));
//        runFun.invoke(fun, Integer::valueOf, "3"); // 编译error: Object is not a functional interface
        Function<String, Integer> innerFun = Integer::valueOf; // 虽然直接不行，但可以封装成Function，然后就可以了
        var list = new ArrayList<Integer>(100000);
        for (int i = 0; i < 100000; i++) {
            list.add(handleInvoke(runFun, fun, innerFun, "" + i));
        }
        System.out.println(list.size());
    }

    Integer handleInvoke(MethodHandle runFun, MultiFunction fun, Function innerFun, String str) throws Throwable {
        return (Integer) runFun.invoke(fun, innerFun, str); // 所以findVirtual也可以应对参数包含lambda的method
    }

    @Test
    void testLambda() throws Throwable { // 复用前4s191ms，函数复用后45ms
        var lookup = MethodHandles.lookup();
        var multiFunction = (MultiFunction) LambdaMetafactory.metafactory(
                lookup,
                "runFun", // interfaceMethodName
                MethodType.methodType(MultiFunction.class), // factoryType
                MethodType.methodType(Integer.class, Function.class, String.class), // interfaceMethodType
                lookup.findVirtual(Function.class, "apply", MethodType.methodType(Object.class, Object.class)), // implementation
                MethodType.methodType(Integer.class, Function.class, String.class) // dynamicMethodType
        ).getTarget().invokeExact(); // 这部分复用，就会快很多
        var list = new ArrayList<Integer>(100000);
        for (int i = 0; i < 100000; i++) {
            list.add(lambdaMeta(multiFunction, "" + i));
        }
        System.out.println(list.size());
    }

    Integer lambdaMeta(MultiFunction multiFunction, String str) {
        return multiFunction.runFun(Integer::valueOf, str);
    }

    @Test
    void testReflect() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException { // 88ms
        MultiFunction fun = Function::apply;
        Function<String, Integer> innerFun = Integer::valueOf; // 虽然直接不行，但可以封装成Function，然后就可以了
        var runFun = MultiFunction.class.getDeclaredMethod("runFun", Function.class, String.class);
        runFun.trySetAccessible(); // 执行这个后，invoke时会略去checkAccess，耗时从88ms -> 60ms，也算是一个技巧
        var list = new ArrayList<Integer>(100000);
        for (int i = 0; i < 100000; i++) {
            list.add(reflectInvoke(runFun, fun, innerFun, "" + i));
        }
        System.out.println(list.size());
    }

    Integer reflectInvoke(Method runFun, MultiFunction fun, Function<String, Integer> innerFun, String str) throws IllegalAccessException, InvocationTargetException {
        return (Integer) runFun.invoke(fun, innerFun, str);
    }
}
