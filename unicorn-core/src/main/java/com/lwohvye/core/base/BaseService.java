/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.core.base;

import com.lwohvye.core.exception.NeedImplementException;

/**
 * Service层部分公共方法
 *
 * @author Hongyan Wang
 * @date 2021/6/17 5:07 下午
 */
public interface BaseService {

    /**
     * 部分场景下，在类初始化完成后，执行部分额外操作
     * 通过抛异常的方式，限制必须手动实现
     *
     * @date 2021/7/18 19:06
     */
    default void doInit() {
        throw new NeedImplementException("Can't use default method, Please Implement it by yourself !");
    }

    default void doRegister() {
        throw new NeedImplementException("Can't use default method, Please Implement it by yourself !");
    }
}
