package com.lwohvye.modules.system.service.impl;

import com.lwohvye.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.modules.system.domain.Menu;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.enums.UserTypeEnum;
import com.lwohvye.modules.system.repository.RoleRepository;
import com.lwohvye.modules.system.service.AUserTypeHandler;
import com.lwohvye.utils.StringUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Hongyan Wang
 * @date 2021年11月02日 19:24
 */
@Component
@RequiredArgsConstructor
@UserTypeHandlerAnno(UserTypeEnum.NORMAL)
public class NormalUserTypeHandler extends AUserTypeHandler {

    private final RoleRepository roleRepository;

    @Override
    public List<GrantedAuthority> handler(Long userId) {
        Set<Role> roles = roleRepository.findByUserId(userId);
        var permissions = roles.stream().flatMap(role -> role.getMenus().stream())
                .map(Menu::getPermission)
                .filter(StringUtils::isNotBlank).collect(Collectors.toSet());
        return permissions.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList());
    }
}
