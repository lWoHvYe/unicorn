package com.lwohvye.config;

import com.lwohvye.config.condition.LinuxCondition;
import com.lwohvye.config.condition.MacOSCondition;
import com.lwohvye.config.condition.WindowsCondition;
import com.lwohvye.modules.system.service.TerminalService;
import com.lwohvye.modules.system.service.local.LinuxTerminalServiceImpl;
import com.lwohvye.modules.system.service.local.MacOSTerminalServiceImpl;
import com.lwohvye.modules.system.service.local.WindowsTerminalServiceImpl;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
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
@ConditionalOnExpression("${local.sys.load-os:false}")
@Configuration
public class TerminalServiceConditionConfig {

    /**
     * 当MacOSCondition方法中的matches返回true的时候，
     * MacOSTerminalService会被注入，否则不注入。
     */
    @Bean("terminalService")
    @Conditional({MacOSCondition.class})
    public TerminalService macOSTerminalService() {
        return new MacOSTerminalServiceImpl();
    }

    /**
     * 当WindowCondition方法中的matches返回true的时候，
     * WindowTerminalService会被注入，否则不注入。
     */
    @Bean("terminalService")
    @Conditional({WindowsCondition.class})
    public TerminalService windowTerminalService() {
        return new WindowsTerminalServiceImpl();
    }


    /**
     * 当LinuxCondition方法中的matches返回true的时候，
     * LinuxTerminalService会被注入，否则不注入。
     */
    @Bean("terminalService")
    @Conditional({LinuxCondition.class})
    public TerminalService linuxTerminalService() {
        return new LinuxTerminalServiceImpl();
    }
}
