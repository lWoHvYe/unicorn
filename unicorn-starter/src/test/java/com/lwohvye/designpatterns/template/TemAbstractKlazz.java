/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

import com.lwohvye.sys.modules.system.strategy.AUserTypeStrategy;

/**
 * 模版方法，抽象父类，与接口型的相比，能做很多抽象类可以做，接口不能做的事。但接口在加新抽象方法时，可整为default，从而不影响子类
 *
 * @date 2022/3/23 5:58 PM
 * @see AUserTypeStrategy
 */
public abstract class TemAbstractKlazz {

    private final String I = "AbstractKlazz";

    /**
     * 定义模板，可设置为final的，禁止子类重写，这是较接口不同的，同时接口中已运行有私有方法，但不能有私有属性
     *
     * @param name
     * @date 2022/3/23 6:08 PM
     */
    public final void sayHello(String name) {
        notice(name);
        beforeSay(name);
        saySomething(name);
        afterSay(name);
    }

    /**
     * 私有方法
     *
     * @param name
     * @date 2022/3/23 6:09 PM
     */
    private void notice(String name) {
        System.out.printf("I is %s，and You is %s%n", I, this.getClass().getName());
    }

    /**
     * 公有普通方法，子类可重写。这里可设置为protected，不让外部直接调用
     *
     * @param name
     * @date 2022/3/23 6:11 PM
     */
    protected void beforeSay(String name) {
        System.out.println("让我想想......");
    }

    protected void afterSay(String name) {
        System.out.println("说完了......");
    }

    /**
     * 抽象方法，由子类实现细节
     *
     * @param name
     * @date 2022/3/23 6:10 PM
     */
    public abstract void saySomething(String name);

}
