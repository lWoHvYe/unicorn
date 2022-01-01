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
package com.lwohvye.modules.config;

import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.modules.mongodb.rest.MongoDBUserController;
import com.lwohvye.utils.SpringContextHolder;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.Objects;

/**
 * @author Hongyan Wang
 * @date 2021年07月18日 18:15
 * @description 当Spring将所有的Bean都初始化完成后，会留有一个入口，通过实现如下接口，可在此阶段进行部分业务
 * 与  @PostConstruct 的区别在于，此时ApplicationContext已可以获取到
 */
@Configuration
public class InstantiationTracingBeanPostProcessor4Search implements ApplicationListener<ContextRefreshedEvent> {
    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        if (Objects.isNull(event.getApplicationContext().getParent())) {//root application context 没有parent，再执行这个.
            //需要执行的逻辑代码，当spring容器初始化完成后就会执行该方法。
            var controller = SpringContextHolder.getBean(MongoDBUserController.class);
            ReflectUtil.invoke(controller, "doInit");
        }
    }
}
