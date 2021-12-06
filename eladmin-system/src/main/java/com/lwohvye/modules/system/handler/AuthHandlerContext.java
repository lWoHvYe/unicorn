/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.modules.system.handler;

import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;

import java.util.Map;

/**
 * 策略模式上下文，该类的注入由相关的HandlerProcessor实现
 *
 * @author Hongyan Wang
 * @date 2021年11月02日 16:33
 * @see AuthHandlerProcessor
 */
@Slf4j
public record AuthHandlerContext(Map<Integer, AUserTypeHandler> handlerMap) {

    /**
     * 获取实例。handlerMap由另一个类来初始化
     *
     * @param userType /
     * @return com.lwohvye.modules.system.handler.AUserTypeHandler
     * @date 2021/11/2 17:10
     */
    public AUserTypeHandler getInstance(Integer userType) {

        log.warn(" van：boy next door,do you like van游戏 ");

        Assert.notNull(userType, "用户类型不可为空");

        var clazz = handlerMap.get(userType);

        Assert.notNull(clazz, "该类型无业务支撑，请期待后续支持");

        return clazz;
    }

}
