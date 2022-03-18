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
package com.lwohvye.modules.security.service;

import com.google.common.util.concurrent.UncheckedExecutionException;
import com.lwohvye.modules.security.config.bean.LoginProperties;
import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.modules.system.service.IDataService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.ExecutionException;

/**
 * @author Zheng Jie
 * @date 2018-11-22
 */
// 这里声明了UserDetailsService的实现使用这一个。因为该接口有多个实现
@Service("userDetailsService")
@RequiredArgsConstructor
public class UserDetailsServiceImpl implements UserDetailsService {

    private final UserLocalCache userLocalCache;
    private final IDataService dataService;
    private final LoginProperties loginProperties;

    public void setEnableCache(boolean enableCache) {
        this.loginProperties.setCacheEnable(enableCache);
    }


    @Override
    public JwtUserDto loadUserByUsername(String username) {
        JwtUserDto jwtUserDto;
        if (loginProperties.isCacheEnable()) {
            try {
                jwtUserDto = userLocalCache.userLRUCache.get(username); // 这个Cache在目标不存在时，会执行定义的获取方法，若方法中抛出异常，就是ExecutionException或者UncheckedExecutionException
            } catch (ExecutionException | UncheckedExecutionException e) { // 当下，这里都是 UncheckedExecutionException
                throw new UsernameNotFoundException(e.getCause() instanceof RuntimeException re ? re.getMessage() : ""); // 这里大多数都是""，因为抛的时候就是这样，所以不写这么麻烦也可以。只要没成功，都按认证失败处理
            }
            var userInner = jwtUserDto.getUser();
            // 检查dataScope是否修改
            List<Long> dataScopes = jwtUserDto.getDataScopes();
            dataScopes.clear();
            dataScopes.addAll(dataService.getDeptIds(userInner.getId(), userInner.getDeptId()));
        } else {
            jwtUserDto = userLocalCache.getUserDB(username);
        }
        return jwtUserDto;
    }
}
