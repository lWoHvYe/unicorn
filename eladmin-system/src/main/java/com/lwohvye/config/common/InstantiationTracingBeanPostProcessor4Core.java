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
package com.lwohvye.config.common;

import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.modules.system.service.ITerminalService;
import com.lwohvye.modules.system.handler.NormalUserTypeHandler;
import com.lwohvye.utils.SpringContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * 当Spring将所有的Bean都初始化完成后，会留有一个入口，通过实现如下接口，可在此阶段进行部分业务
 * 与  @PostConstruct 的区别在于，此时ApplicationContext已可以获取到
 *
 * @author Hongyan Wang
 * @date 2021年07月18日 18:15
 */
@Slf4j
@Configuration
public class InstantiationTracingBeanPostProcessor4Core implements ApplicationListener<ContextRefreshedEvent> {
    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        if (event.getApplicationContext().getParent() == null) {//root application context 没有parent，再执行这个.
            //需要执行的逻辑代码，当spring容器初始化完成后就会执行该方法。
            var userTypeHandler = SpringContextHolder.getBean(NormalUserTypeHandler.class);
            ReflectUtil.invoke(userTypeHandler, "doInit");

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
