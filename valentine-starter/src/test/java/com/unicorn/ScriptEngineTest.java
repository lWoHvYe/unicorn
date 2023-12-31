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

package com.unicorn;

import org.junit.jupiter.api.Test;

import javax.script.*;

class ScriptEngineTest {


    @Test
    void testEngine() {

        ScriptEngineManager manager = new ScriptEngineManager();
//        ScriptEngine engine = manager.getEngineByName("Graal.js");
        ScriptEngine engine = manager.getEngineByName("javascript");

        try {
            Object result = engine.eval("1 + 2");
            System.out.println("Result: " + result); // 输出：Result: 3
        } catch (ScriptException e) {
            e.printStackTrace();
        }


        Bindings bindings = engine.createBindings();
        bindings.put("x", 5);
        bindings.put("y", 14);
        try {
            Object result = engine.eval("x * 10 + y", bindings);
            System.out.println("Result: " + result); // 输出：Result: 50
        } catch (ScriptException e) {
            e.printStackTrace();
        }

    }

    @Test
    void testGraalVMEngine() throws ScriptException {
//        To avoid unnecessary re-compilation of JS sources, it is recommended to use CompiledScript.eval instead of ScriptEngine.eval.
//        This prevents JIT-compiled code from being garbage-collected as long as the corresponding CompiledScript object is alive.
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("js");

        CompiledScript script = ((Compilable) engine).compile("console.log('hello world');");
        script.eval();
        CompiledScript script1 = ((Compilable) engine).compile("1 + 2");
        Object result = script1.eval();
        System.out.println("Result: " + result); // 输出：Result: 3

        CompiledScript script2 = ((Compilable) engine).compile("console.log('start');var start = Date.now(); while (Date.now()-start < 2000);console.log('end');");
        Thread.ofVirtual().start(() -> {
            try {
                // Create ScriptEngine for this thread (with a shared polyglot Engine)
                ScriptEngine innerEngine = manager.getEngineByName("js");
                script2.eval(innerEngine.getContext());
            } catch (ScriptException scriptException) {
                scriptException.printStackTrace();
            }
        });
        script2.eval();

    }
}
