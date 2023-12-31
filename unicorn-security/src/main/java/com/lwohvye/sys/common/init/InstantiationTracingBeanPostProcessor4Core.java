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
package com.lwohvye.sys.common.init;

import com.lwohvye.core.utils.JDKUtils;
import com.lwohvye.core.utils.SpringContextHolder;
import com.lwohvye.sys.modules.system.service.ITerminalService;
import com.lwohvye.sys.modules.system.strategy.NormalUserTypeStrategy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

/**
 * 当Spring将所有的Bean都初始化完成后，会留有一个入口，通过实现如下接口，可在此阶段进行部分业务
 * 与  @PostConstruct 的区别在于，此时ApplicationContext已可以获取到
 * <p>
 * Spring 中@Configuration 和 @Component 区别：
 * `@Configuration` 注解本质上还是 `@Component`
 * `@Configuration` 中所有带 `@Bean` 注解的方法都会被动态代理，因此调用该方法返回的都是同一个实例。
 * `@Component` 注解并没有通过 cglib 来代理`@Bean` 方法的调用，因此调用该方法返回的是不同的实例。
 * 或者简单一点，里面有定义Bean的，使用`@Configuration` 。
 *
 * @author Hongyan Wang
 * @date 2021年07月18日 18:15
 */
@Slf4j
@Component
public class InstantiationTracingBeanPostProcessor4Core implements ApplicationListener<ContextRefreshedEvent> {
    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        if (event.getApplicationContext().getParent() == null) {//root application context 没有parent，再执行这个.
            //需要执行的逻辑代码，当spring容器初始化完成后就会执行该方法。这种可以参考观察者模式改造中的方式，先addCallBacks，启动后会自动执行，这里用来保留另一种方式。
            var userTypeHandler = SpringContextHolder.getBean(NormalUserTypeStrategy.class);
            JDKUtils.invokeMethod(userTypeHandler, "doInit", void.class);

            var terminalClazz = ITerminalService.class;
            try {
                var terminalService = SpringContextHolder.getBean(terminalClazz);
                terminalService.prSysName();
            } catch (BeansException e) {
                log.warn("获取bean出错，beanName: {} || errMsg: {} ", terminalClazz.getSimpleName(), e.getMessage());
            }
        }
    }
}
