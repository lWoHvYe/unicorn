package com.lwohvye.modules.system.service.impl;

import com.lwohvye.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.modules.system.enums.UserTypeEnum;
import com.lwohvye.modules.system.service.AUserTypeHandler;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Hongyan Wang
 * @date 2021年11月02日 19:25
 */
@Component
@UserTypeHandlerAnno(UserTypeEnum.DEV)
public class DevUserTypeHandler extends AUserTypeHandler {
    @Override
    public List<GrantedAuthority> handler(Long userId) {
        Set<String> permissions = new HashSet<>();
        // 这里只是随便写一下，正常是走不到这个handler的
        permissions.add("admin-dev");
        return permissions.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList());
    }
}
