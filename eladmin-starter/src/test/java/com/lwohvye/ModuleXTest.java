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

package com.lwohvye;

import org.junit.jupiter.api.Test;

public class ModuleXTest {

    // 模块API https://www.lwohvye.com/2022/03/17/jpms-%e6%a8%a1%e5%9d%97api/
    @Test
    public void moduleInfo() {
        Class<? extends ModuleXTest> aClass = this.getClass();
        var classLoader = aClass.getClassLoader();
        var module = aClass.getModule();
    }
}
