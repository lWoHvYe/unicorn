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
package com.lwohvye.sys.modules.system.strategy;

import com.lwohvye.sys.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.sys.modules.system.enums.UserTypeEnum;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.core.utils.SpringContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author Hongyan Wang
 * @date 2021年11月02日 19:24
 */
@Slf4j
@Component
// 不能用下面这个注解，因为这个的使用方式，决定了要使用空参构造初始化。对于需要注入的对象，需特殊处理
//@RequiredArgsConstructor
@UserTypeHandlerAnno(UserTypeEnum.NORMAL)
public final class NormalUserTypeStrategy implements AUserTypeStrategy {

    @Autowired
    private IRoleService roleService;

    /**
     * 属性注入。这里不使用@PostConstruct后置处理，是因为之前有验证在执行后置处理的时候，SpringContextHolder还无法获取到相关的bean（因为applicationContext还未注入）
     *
     * @date 2022/3/13 6:03 PM
     */
    @Override
    public void doInit() {
        if (Objects.isNull(roleService)) // 这里只是为了适配一些情况，当前默认情况是不需要这部分init的，但通过调整参数使用PostProcessor来Inject StrategyBean时，field会是null，此时需要这块逻辑
            this.roleService = SpringContextHolder.getBean(IRoleService.class);
    }

    @Override
    public List<GrantedAuthority> grantedAuth(Long userId) {
        log.warn(" banana：自由的气息，蕉迟但到。");
        var roles = roleService.findByUserId(userId);
        var permissions = roles.stream().map(role -> "ROLE_" + role.getCode().toUpperCase()).collect(Collectors.toSet());
        // .flatMap(role -> role.getResources().stream())
        // .map(Resource::getPattern)
        // .filter(StringUtils::isNotBlank).collect(Collectors.toSet());
        return permissions.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList());
    }
}
