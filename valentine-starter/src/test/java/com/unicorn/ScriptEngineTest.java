/*
 *    Copyright (c) 2023.  lWoHvYe(Hongyan Wang)
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

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

class ScriptEngineTest {


    @Test
    void testEngine() {

        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("Graal.js");

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
}
