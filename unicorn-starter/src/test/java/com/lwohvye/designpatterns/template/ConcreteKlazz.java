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

package com.lwohvye.designpatterns.template;

public class ConcreteKlazz extends TemAbstractKlazz {
    /**
     * 抽象方法，由子类实现细节
     *
     * @param name
     * @date 2022/3/23 6:10 PM
     */
    @Override
    public void saySomething(String name) {
        System.out.println("社会主义核心价值观");
    }

    /**
     * 子类可以覆盖父类的实现
     *
     * @param name
     * @date 2022/3/23 6:17 PM
     */
    @Override
    protected void beforeSay(String name) {
        super.beforeSay(name);
        System.out.printf("%s 请等一下....%n", name);
    }

}
