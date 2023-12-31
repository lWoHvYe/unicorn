/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.modules.system.strategy;

/**
 * 留给外部的一个扩展点，通过继承该类并添加注解来扩展UserType。另外父类为密封类，该子类标记为了非密封类
 *
 * @date 2022/5/1 2:44 PM
 */
public non-sealed interface ExtraUserTypeStrategy extends AUserTypeStrategy {
}
