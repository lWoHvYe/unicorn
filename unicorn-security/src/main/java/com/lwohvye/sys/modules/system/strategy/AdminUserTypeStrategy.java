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

import com.lwohvye.core.constant.SecurityConstant;
import com.lwohvye.sys.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.sys.modules.system.enums.UserTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Hongyan Wang
 * @date 2021年11月02日 19:22
 */
@Slf4j
@Component
@UserTypeHandlerAnno(UserTypeEnum.ADMIN)
public final class AdminUserTypeStrategy implements AUserTypeStrategy {
    @Override
    public List<GrantedAuthority> grantedAuth(Long userId) {
        log.warn(" billy：吾乃新日暮里的王，三界哲学的主宰。");
        Set<String> permissions = new HashSet<>();
        permissions.add(SecurityConstant.ROLE_ADMIN);
        return permissions.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList());
    }

    @Override
    public void sayHello(Long userId) {
        System.out.printf("%s ，您好。%n", UserTypeEnum.ADMIN.getDesc());
    }

    @Override
    public void sayBye(Long userId) {
        // AUserTypeStrategy.super.sayBye(userId); 可以像这样调用父类公开的方法
        System.out.printf("%s ，期待您的下次光临。%n", UserTypeEnum.ADMIN.getDesc());
    }
}
