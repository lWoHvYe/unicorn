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

package com.lwohvye.designpatterns.decorator;

// 抽象装饰角色
// 抽象类实现接口，原接口中的方法，可以通过抽象方法的方式，在抽象类中进行初步实现
abstract class ActivityDecorator implements ActivityInterface {
    protected ActivityInterface activity;

    public ActivityDecorator(ActivityInterface activity) {
        this.activity = activity;
    }

    @Override
    public abstract void participate(Long userId);
}
