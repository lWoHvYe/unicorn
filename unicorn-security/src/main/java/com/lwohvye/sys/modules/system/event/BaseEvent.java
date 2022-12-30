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

package com.lwohvye.sys.modules.system.event;

import lombok.Getter;
import lombok.SneakyThrows;
import org.springframework.context.ApplicationEvent;

import java.lang.invoke.MethodHandles;

/**
 * <a href="https://docs.spring.io/spring-framework/docs/current/reference/html/core.html#spring-core">Doc Page</a>
 * 有空好好看一下这个文档
 * 与观察者模式相比，Event可能更简单一些，在一些方面也灵活，且@EventListener支持SpEL表达式，可以做不少富有想象力的事情，
 * 但观察者模式有注册观察者和注销观察者，这个是可以在Runtime做的，可以在一些特殊的场景做一些特殊的事情，这点Event应该不太好实现
 *
 * @date 2022/7/16 5:31 PM
 */
@Getter
public abstract class BaseEvent<T> extends ApplicationEvent {

    /**
     * 该类型事件携带的信息
     */
    private T eventData;

    /**
     * @param source    最初触发该事件的对象
     * @param eventData 该类型事件携带的信息
     */
    protected BaseEvent(Object source, T eventData) {
        super(source);
        this.eventData = eventData;
    }

    @SneakyThrows // 这个注解，本质是try-catch后再throw。好像有说法是引入try-catch并不会影响性能。没有在构造中设置id属性是考虑到有的可能没这属性
    public Long getDataId() {
        // 这个findGetter就是获取属性，不是找getter方法，别搞混了。这里需要用privateLookupIn
        // return (Long) MethodHandles.lookup().findGetter(eventData.getClass(), "id", Long.class).invoke(eventData);
        var aClass = eventData.getClass();
        // 有定义public的getter的话，可以用这个
        // return (Long) MethodHandles.lookup().findVirtual(aClass, "getId", MethodType.methodType(Long.class)).invoke(eventData); // 1.7的方式
        return (Long) MethodHandles.privateLookupIn(aClass, MethodHandles.lookup()).findVarHandle(aClass, "id", Long.class).get(eventData); // 1.9的方式
    }
}
