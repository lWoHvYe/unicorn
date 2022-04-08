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

package com.lwohvye.sys.modules.system.subject;

import com.lwohvye.sys.modules.system.observer.UserObserver;

import java.util.ArrayList;
import java.util.List;

// 抽象目标
public abstract class UserSubject {
    protected List<UserObserver> userObservers = new ArrayList<>();

    // 增加观察者方法
    public void addObserver(UserObserver userObserver) {
        userObservers.add(userObserver);
    }

    // 删除观察者方法
    public void removeObserver(UserObserver userObserver) {
        userObservers.remove(userObserver);
    }

    // 通知观察者方法
    public void notifyObserver(Object obj) {
        for (var userObserver : userObservers) {
            userObserver.userUpdate(obj);
        }
    }
}
