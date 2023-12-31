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
package com.lwohvye.sys.common.config;

import com.lwohvye.sys.common.condition.LinuxCondition;
import com.lwohvye.sys.common.condition.MacOSCondition;
import com.lwohvye.sys.common.condition.WindowsCondition;
import com.lwohvye.sys.modules.system.service.ITerminalService;
import com.lwohvye.sys.modules.system.service.local.LinuxTerminalServiceImpl;
import com.lwohvye.sys.modules.system.service.local.MacOSTerminalServiceImpl;
import com.lwohvye.sys.modules.system.service.local.WindowsTerminalServiceImpl;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;

/**
 * 条件配置类.
 *
 * @author Angel --守护天使
 * @version v.0.1
 * @date 2017年8月23日
 */
// Condition系列可用在bean或者class上
//@Conditional({DoLoadOSCondition.class})
//@ConditionalOnProperty(prefix = "local.sys", name = "load-os", havingValue = "true")
@ConditionalOnProperty(prefix = "local.sys", name = "load-os")
@Configuration
public class TerminalServiceConditionConfig {

    /**
     * 当MacOSCondition方法中的matches返回true的时候，
     * MacOSTerminalService会被注入，否则不注入。
     */
    @Bean("terminalService")
    @Conditional({MacOSCondition.class})
    public ITerminalService macOSTerminalService() {
        return new MacOSTerminalServiceImpl();
    }

    /**
     * 当WindowCondition方法中的matches返回true的时候，
     * WindowTerminalService会被注入，否则不注入。
     */
    @Bean("terminalService")
    @Conditional({WindowsCondition.class})
    public ITerminalService windowTerminalService() {
        return new WindowsTerminalServiceImpl();
    }


    /**
     * 当LinuxCondition方法中的matches返回true的时候，
     * LinuxTerminalService会被注入，否则不注入。
     */
    @Bean("terminalService")
    @Conditional({LinuxCondition.class})
    public ITerminalService linuxTerminalService() {
        return new LinuxTerminalServiceImpl();
    }
}
