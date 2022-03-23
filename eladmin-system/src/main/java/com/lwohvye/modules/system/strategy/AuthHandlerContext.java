/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.modules.system.strategy;

import com.lwohvye.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.utils.SpringContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import javax.annotation.PostConstruct;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * 策略模式上下文（环境类），给外部调用，该类的注入由相关的HandlerProcessor实现
 *
 * @author Hongyan Wang
 * @date 2021年11月02日 16:33
 * @see AuthHandlerProcessor
 */
@Slf4j
@Component
@ConditionalOnExpression("!${local.sys.init-bf:false}")
public class AuthHandlerContext {

    Map<Integer, AUserTypeStrategy> strategyMap;

    // BeanPostProcessor是在spring容器加载了bean的定义文件并且实例化bean之后执行的。BeanPostProcessor的执行顺序是在BeanFactoryPostProcessor之后。
    // 当使用BeanFactoryPostProcessor来注入属性时，这个后置处理是不会执行到的
    @PostConstruct
    public void doInit() {
        SpringContextHolder.addCallBacks(this::initStrategyMap);
    }

    public void initStrategyMap() {
        if (Objects.isNull(strategyMap)) {
            synchronized (this) {
                if (Objects.isNull(strategyMap)) {
                    strategyMap = new HashMap<>();
                    var tCollection = SpringContextHolder.getBeansOfType(AUserTypeStrategy.class).values();
                    for (var t : tCollection) {
                        var userTypeHandlerAnno = t.getClass().getAnnotation(UserTypeHandlerAnno.class);
                        if (ObjectUtils.isEmpty(userTypeHandlerAnno)) continue;
                        strategyMap.put(userTypeHandlerAnno.value().getType(), t);
                    }
                }
            }
        }
    }

    /**
     * 获取实例。handlerMap由另一个类来初始化
     *
     * @param userType /
     * @return com.lwohvye.modules.system.handler.AUserTypeHandler
     * @date 2021/11/2 17:10
     */
    public AUserTypeStrategy getInstance(Integer userType) {

        log.warn(" van：boy next door,do you like van游戏 ");

        Assert.notNull(userType, "用户类型不可为空");

        var clazz = strategyMap.get(userType);

        Assert.notNull(clazz, "该类型无业务支撑，请期待后续支持");

        return clazz;
    }

}
