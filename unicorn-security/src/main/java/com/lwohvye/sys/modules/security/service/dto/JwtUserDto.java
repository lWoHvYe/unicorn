/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.sys.modules.security.service.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.lwohvye.api.modules.system.service.dto.UserInnerDto;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Spring Security鉴权使用的UserDetails，
 *
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Getter
@AllArgsConstructor
public class JwtUserDto implements UserDetails {

    private final UserInnerDto user;

    // 不做序列化，使用时进行转换
    @JsonIgnore
    /*
        修饰符transient可以应用于类的字段成员，以关闭这些字段成员的序列化。
        你可以在需要对现有状态字段进行保护或计算的字段的类中使用transient关键字。当序列化那些字段(如日志记录器和线程)毫无意义时，可以使用它。
        序列化不关心访问修饰符，如private；所有非transient字段都被认为是对象持久状态的一部分，并且都符合持久状态的条件。
        无论何时将任何final字段/引用计算为“常量表达式”，JVM都会对其进行序列化，忽略transient关键字的存在。比如 private final transient String = "abc"，就还会被序列化
        HashMap类是java中transient关键字的一个很好的用例
     */
    private final transient List<GrantedAuthority> authorities;

    // 可能名称易被误解，这里的roles是用户的权限信息，类似与permission，通过该属性，前端判断🔘的显示等，没有该属性就不会在登录后跳转首页。所以不能注释掉
    public Set<String> getRoles() {
        return authorities.stream().map(GrantedAuthority::getAuthority).collect(Collectors.toSet());
    }

    @Override
    @JsonIgnore
    public String getPassword() {
        return user.getPassword();
    }

    @Override
    @JsonIgnore
    public String getUsername() {
        return user.getUsername();
    }

    @Override
    @JsonIgnore
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    @JsonIgnore
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    @JsonIgnore
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    @JsonIgnore
    public boolean isEnabled() {
        return user.getEnabled();
    }

    // endregion
}
