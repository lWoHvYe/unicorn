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
package com.lwohvye.sys.modules.system.strategy;

import com.lwohvye.sys.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.sys.modules.system.enums.UserTypeEnum;
import com.lwohvye.core.utils.SpringContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * 策略模式上下文（环境类），给外部调用，该类的注入可由相关的HandlerProcessor实现（Has Deprecated），当前改为通过initStrategyMap()来实现Init @ Inject
 *
 * @author Hongyan Wang
 * @date 2021年11月02日 16:33
 * @see AuthHandlerProcessor
 */
@Slf4j
@Component
@ConditionalOnExpression("!${local.sys.init-bf:false}")
public class AuthHandlerContext {

    final Map<Integer, AUserTypeStrategy> strategyMap;

    public AuthHandlerContext() {
        strategyMap = new HashMap<>();
        SpringContextHolder.addCallBacks(this::initStrategyMap);
    }

    public AuthHandlerContext(Map<Integer, AUserTypeStrategy> strategyMap) {
        this.strategyMap = strategyMap;
    }

    // BeanPostProcessor是在spring容器加载了bean的定义文件并且实例化bean之后执行的。BeanPostProcessor的执行顺序是在BeanFactoryPostProcessor之后。
    // 当使用BeanFactoryPostProcessor来注入属性时，这个后置处理是不会执行到的
    // Because AOP auto-proxying is implemented as a BeanPostProcessor itself, neither BeanPostProcessor instances nor the beans they directly reference
    // are eligible for auto-proxying and, thus, do not have aspects woven into them.
    // @PostConstruct
    // public void doInit() {
    // }

    /**
     * 这里主要进行相关bean等注入，较 AuthHandlerProcessor 中的方式，解决了注入的bean中属性为null的问题
     *
     * @date 2022/8/21 5:49 PM
     */
    public void initStrategyMap() {
        var tCollection = SpringContextHolder.getBeansOfType(AUserTypeStrategy.class).values();
        for (var t : tCollection) {
            var userTypeHandlerAnno = t.getClass().getAnnotation(UserTypeHandlerAnno.class);
            if (Objects.equals(userTypeHandlerAnno.value(), UserTypeEnum.EXTRA)) {
                var typeName = userTypeHandlerAnno.typeName();
                if (StringUtils.hasText(typeName)) {
                    strategyMap.put(UserTypeEnum.valueOf(typeName).getType(), t);
                }
            } else {
                strategyMap.put(userTypeHandlerAnno.value().getType(), t);
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

    public void switchPatternMatchingTest(AUserTypeStrategy userTypeStrategy) {
        switch (userTypeStrategy) { // Since Java 19，这个不是纯粹的语法糖，可以看看编译的class
            case AdminUserTypeStrategy __ -> System.out.println(__.getSysName()); // _ 是可以做变量名(的一部分)的，虽然好像不推荐
            case DevUserTypeStrategy __ -> System.out.println("dev");
            case NormalUserTypeStrategy norm -> System.out.println(norm.getSysName());
            case ExtraUserTypeStrategy ignored -> System.out.println("ext");
        }
    }

    /*public void switchPatternMatchingTest(AUserTypeStrategy userTypeStrategy) {
        Objects.requireNonNull(userTypeStrategy);
        byte var3 = 0;
        switch (userTypeStrategy.typeSwitch<invokedynamic>(userTypeStrategy, var3)) {
            case 0:
                AdminUserTypeStrategy __ = (AdminUserTypeStrategy)userTypeStrategy;
                System.out.println(__.getSysName());
                break;
            case 1:
                DevUserTypeStrategy __ = (DevUserTypeStrategy)userTypeStrategy;
                System.out.println("dev");
                break;
            case 2:
                NormalUserTypeStrategy norm = (NormalUserTypeStrategy)userTypeStrategy;
                System.out.println(norm.getSysName());
                break;
            case 3:
                ExtraUserTypeStrategy ignored = (ExtraUserTypeStrategy)userTypeStrategy;
                System.out.println("ext");
                break;
            default:
                throw new MatchException((String)null, (Throwable)null);
        }

    }*/

}
