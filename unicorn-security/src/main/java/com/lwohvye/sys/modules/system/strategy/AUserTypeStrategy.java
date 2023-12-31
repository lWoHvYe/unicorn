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
package com.lwohvye.sys.modules.system.strategy;

import com.lwohvye.core.base.BaseService;
import org.springframework.security.core.GrantedAuthority;

import java.util.List;

/**
 * 业务处理基类，密封类sealed。需通过permits指定子类。子类可以是final标记的实现类、sealed标记的密封类、non-sealed标记的非密封类
 * 策略 [ˈstrætədʒi]
 * 策略模式，在这里的使用，就是分域，将同一类型的业务对象的各种处理逻辑放在一起，不同业务对象分离开，方便维护且后续添加新的业务对象也很方便（主体添加新的实现即可）
 *
 * @author Hongyan Wang
 * @date 2021年11月02日 16:42
 */
public sealed interface AUserTypeStrategy extends BaseService permits AdminUserTypeStrategy, DevUserTypeStrategy, ExtraUserTypeStrategy, NormalUserTypeStrategy {

    List<GrantedAuthority> grantedAuth(Long userId);

    // region 留给子类扩展
    default void saySomething(Long userId) {
        doNothing(userId);
    }

    default void beforeSay(Long userId) {
        System.out.println("让我想想......");
    }

    default void afterSay(Long userId) {
        System.out.println("说完了......");
    }
    // endregion

    // region 给外部调用
    // 使用default可以在扩展的同时，减少对下层各实现的影响，这里也算是一种模板方法
    default void sayHello(Long userId) {
        beforeSay(userId);
        saySomething(userId); // 这也是一种设计方式，预留一些扩展点，子类可进行扩展
        System.out.println("哇，你终于回来了～");
        afterSay(userId);
    }

    default void sayBye(Long userId) {
        beforeSay(userId);
        saySomething(userId);
        System.out.println("我们，还能再见面吗......");
        afterSay(userId);
    }
    // endregion

    // 接口中可以有private方法，
    private void doNothing(Long ignoredUserId) {
        System.out.println("...");
    }
}
