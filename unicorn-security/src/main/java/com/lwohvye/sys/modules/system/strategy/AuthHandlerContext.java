/*
 *    Copyright (c) 2021-2023.  lWoHvYe(Hongyan Wang)
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

import com.lwohvye.core.utils.SpringContextHolder;
import com.lwohvye.sys.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.sys.modules.system.enums.UserTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.HashMap;
import java.util.Map;

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
            if (ObjectUtils.isEmpty(userTypeHandlerAnno)) {
                log.warn(" {} 类的 @UserTypeHandlerAnno 注解没有写入值 ", t.getClass().getSimpleName());
                continue;
            }
            final var typeName = userTypeHandlerAnno.typeName();
            final var userType = userTypeHandlerAnno.value();
            switch (t) {
                // 每种类型可以有独立的逻辑。另外这里提供了一个扩展点，就是可以覆盖已有的userType的实现，只需要建立extra的subClass并指定typeName就行了
                case ExtraUserTypeStrategy extraStrategy when StringUtils.hasText(typeName) ->
                        strategyMap.put(UserTypeEnum.valueOf(typeName).getType(), extraStrategy);
                case AdminUserTypeStrategy adminStrategy -> strategyMap.put(userType.getType(), adminStrategy);
                case DevUserTypeStrategy devStrategy -> strategyMap.put(userType.getType(), devStrategy);
                case NormalUserTypeStrategy normalStrategy -> strategyMap.put(userType.getType(), normalStrategy);
                default -> throw new IllegalStateException("Unexpected value: " + t);
            }
            // 下面这种更灵活了，当type是extra时拿typeName，别的都是直接放进去，上面还限定下类型，但引入sealed-class后，没啥区别了
//            if (Objects.equals(userTypeHandlerAnno.value(), UserTypeEnum.EXTRA)) {
//                var typeName = userTypeHandlerAnno.typeName();
//                if (StringUtils.hasText(typeName)) {
//                    strategyMap.put(UserTypeEnum.valueOf(typeName).getType(), t);
//                }
//            } else {
//                strategyMap.put(userTypeHandlerAnno.value().getType(), t);
//            }
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
        switch (userTypeStrategy) { // Since Java 19，这个不是纯粹的语法糖，可以看看编译的class，这个when就很灵性，虽然是颗🍬
            case AdminUserTypeStrategy aut when aut.getSysName().isBlank() ->
                    System.out.println(aut.getSysName()); // _ 是可以做变量名(的一部分)的，虽然好像不推荐
            case DevUserTypeStrategy ignored -> System.out.println("dev");
            case NormalUserTypeStrategy norm -> System.out.println(norm.getSysName());
            case ExtraUserTypeStrategy ignored -> System.out.println("ext");
            default -> throw new IllegalStateException("Unexpected value: " + userTypeStrategy);
        }
    }

    /*
    public void switchPatternMatchingTest(AUserTypeStrategy userTypeStrategy) {
        Objects.requireNonNull(userTypeStrategy);
        AUserTypeStrategy var2 = userTypeStrategy;
        byte var3 = 0;

        while(true) {
            switch (var2.typeSwitch<invokedynamic>(var2, var3)) {
                case 0:
                    AdminUserTypeStrategy aut = (AdminUserTypeStrategy)var2;
                    if (!aut.getSysName().isBlank()) {
                        var3 = 1;
                        continue;
                    }

                    System.out.println(aut.getSysName());
                    break;
                case 1:
                    DevUserTypeStrategy ignored = (DevUserTypeStrategy)var2;
                    System.out.println("dev");
                    break;
                case 2:
                    NormalUserTypeStrategy norm = (NormalUserTypeStrategy)var2;
                    System.out.println(norm.getSysName());
                    break;
                case 3:
                    ExtraUserTypeStrategy ignored = (ExtraUserTypeStrategy)var2;
                    System.out.println("ext");
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + String.valueOf(userTypeStrategy));
            }

            return;
        }
    }*/

}
