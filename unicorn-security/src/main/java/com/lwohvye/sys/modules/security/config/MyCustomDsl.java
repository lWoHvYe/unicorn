/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.modules.security.config;

import com.lwohvye.core.utils.JDKUtils;
import com.lwohvye.sys.modules.security.core.filter.CustomAuthenticationCaptchaFilter;
import com.lwohvye.sys.modules.security.core.filter.CustomAuthenticationFilter;
import com.lwohvye.core.utils.SpringContextHolder;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

/**
 * @date 2022/5/20 9:39 AM
 * @link <a href="https://stackoverflow.com/questions/72014162/how-to-migrate-deprecated-websecurityconfigureradapter-to-securityfilterchain">...</a>
 */
@RequiredArgsConstructor
public class MyCustomDsl extends AbstractHttpConfigurer<MyCustomDsl, HttpSecurity> {
    private final AuthenticationSuccessHandler authenticationSuccessHandler;
    private final AuthenticationFailureHandler authenticationFailureHandler;

    @Override
    public void configure(HttpSecurity http) {
        // SharedObject是Spring Security提供的一个非常好用的功能，如果需要在不同的地方需要对一个对象重复使用就可以将它注册为SharedObject，甚至直接注入Spring IoC像下面这样获取就可以了。
        var authenticationManager = http.getSharedObject(AuthenticationManager.class);
        // while the CaptchaService not exist, will verify without captcha
        var useCaptcha = JDKUtils.checkFuncEnable("com.anji.captcha.service.CaptchaService");
        var customAuthenticationFilter = useCaptcha ? new CustomAuthenticationCaptchaFilter() : new CustomAuthenticationFilter();

        customAuthenticationFilter.setAuthenticationSuccessHandler(authenticationSuccessHandler);
        customAuthenticationFilter.setAuthenticationFailureHandler(authenticationFailureHandler);
        customAuthenticationFilter.setFilterProcessesUrl("/auth/login");
        customAuthenticationFilter.setAuthenticationManager(authenticationManager);
        // 不再以bean的形式管理customFilter后，@PostConstruct就不会生效了，这样应该可以注入
        SpringContextHolder.addCallBacks(customAuthenticationFilter::doRegister);

        //用重写的Filter替换掉原有的UsernamePasswordAuthenticationFilter
        // （这里实际上是放到了前面，security自带的Filter在轮到自己执行的时候，会判断当前登录状态，如果已经被之前的Filter验证过了，自己这关就直接放行）
        http.addFilterAt(customAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

    }

    public static MyCustomDsl customDsl(AuthenticationSuccessHandler authenticationSuccessHandler, AuthenticationFailureHandler authenticationFailureHandler) {
        return new MyCustomDsl(authenticationSuccessHandler, authenticationFailureHandler);
    }
}
