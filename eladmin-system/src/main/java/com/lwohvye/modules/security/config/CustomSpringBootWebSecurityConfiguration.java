package com.lwohvye.modules.security.config;

import lombok.SneakyThrows;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

/**
 * @author Hongyan Wang
 * @date 2021/11/28 10:24 上午
 * @description 多个策略配置。欲使用，可在配置文件中开启
 * @see org.springframework.boot.autoconfigure.security.servlet.SpringBootWebSecurityConfiguration
 */
@ConditionalOnExpression("${local.sys.multi-security:false}")
@Configuration
@EnableGlobalMethodSecurity(prePostEnabled = true, jsr250Enabled = true, securedEnabled = true)
@EnableWebSecurity
@ConditionalOnClass(WebSecurityConfigurerAdapter.class)
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
public class CustomSpringBootWebSecurityConfiguration {

    /**
     * 后台接口安全策略. 默认配置
     */
    @Configuration
    @Order(2) // 1似乎被用了，称为100。所以用的2
    @ConditionalOnExpression("${local.sys.multi-security:false}")
    static class AdminConfigurerAdapter extends WebSecurityConfigurerAdapter {

        @Override
        protected void configure(AuthenticationManagerBuilder auth) {
            var daoAuthenticationProvider = new DaoAuthenticationProvider();
            //用户详情服务个性化
            daoAuthenticationProvider.setUserDetailsService(username -> {
                // 自行实现获取UserDetails逻辑
                return null;
            });
            // 也可以设计特定的密码策略
            var bCryptPasswordEncoder = new BCryptPasswordEncoder();
            daoAuthenticationProvider.setPasswordEncoder(bCryptPasswordEncoder);
            auth.authenticationProvider(daoAuthenticationProvider);
        }

        @SneakyThrows
        @Override
        public void configure(WebSecurity web) {
            super.configure(web);
        }

        @Override
        protected void configure(HttpSecurity http) throws Exception {
            // 根据需求自行定制。首个antMatcher指定了该配置生效的范围
            http.antMatcher("/admin/v2")
                    .sessionManagement(Customizer.withDefaults())
                    .formLogin(Customizer.withDefaults());


        }
    }

    /**
     * app接口安全策略. 没有{@link Order}注解优先级比上面低
     */
    @Configuration
    @ConditionalOnExpression("${local.sys.multi-security:false}")
    static class AppConfigurerAdapter extends WebSecurityConfigurerAdapter {

        @Override
        protected void configure(AuthenticationManagerBuilder auth) {
            var daoAuthenticationProvider = new DaoAuthenticationProvider();
            //用户详情服务个性化
            daoAuthenticationProvider.setUserDetailsService(username -> {
                // 自行实现获取UserDetails逻辑
                return null;
            });
            // 也可以设计特定的密码策略
            var bCryptPasswordEncoder = new BCryptPasswordEncoder();
            daoAuthenticationProvider.setPasswordEncoder(bCryptPasswordEncoder);
            auth.authenticationProvider(daoAuthenticationProvider);
        }

        @SneakyThrows
        @Override
        public void configure(WebSecurity web) {
            super.configure(web);
        }

        @Override
        protected void configure(HttpSecurity http) throws Exception {
            // 根据需求自行定制。首个antMatcher指定了该配置生效的范围
            http.antMatcher("/app/v2")
                    .sessionManagement(Customizer.withDefaults())
                    .formLogin(Customizer.withDefaults());
        }
    }
}
