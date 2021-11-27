package com.lwohvye.modules.system.handler;

import com.lwohvye.constant.SecurityConstant;
import com.lwohvye.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.modules.system.enums.UserTypeEnum;
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
public final class AdminUserTypeHandler implements AUserTypeHandler {
    @Override
    public List<GrantedAuthority> handler(Long userId) {
        log.warn(" billy：吾乃新日暮里的王，三界哲学的主宰。");
        Set<String> permissions = new HashSet<>();
        permissions.add(SecurityConstant.ROLE_ADMIN);
        return permissions.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList());
    }
}
