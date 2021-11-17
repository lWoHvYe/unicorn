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

import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.exception.EntityNotFoundException;
import com.lwohvye.modules.security.config.bean.LoginProperties;
import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.modules.security.utils.SecuritySysUtil;
import com.lwohvye.modules.system.service.DataService;
import com.lwohvye.modules.system.service.UserService;
import com.lwohvye.modules.system.service.dto.UserInnerDto;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * @author Zheng Jie
 * @date 2018-11-22
 */
// 这里声明了UserDetailsService的实现使用这一个。因为该接口有多个实现
@Service("userDetailsService")
@RequiredArgsConstructor
public class UserDetailsServiceImpl implements UserDetailsService {
    private final UserService userService;
    private final DataService dataService;
    private final RedisUtils redisUtils;
    private final LoginProperties loginProperties;

    public void setEnableCache(boolean enableCache) {
        this.loginProperties.setCacheEnable(enableCache);
    }


    @Override
    public JwtUserDto loadUserByUsername(String username) {
        boolean searchDb = true;
        JwtUserDto jwtUserDto = null;
        if (loginProperties.isCacheEnable() && redisUtils.hHasKey(SecuritySysUtil.getUserCacheKey(), username)) {
//            jwtUserDto = userDtoCache.get(username);
            // 使用fastjson自带的FastJsonRedisSerializer时，从redis中取出的是JSON（JSONObject、JSONArray）对象
            var cacheUserObj = redisUtils.hGet(SecuritySysUtil.getUserCacheKey(), username);
            if (Objects.isNull(cacheUserObj))
                return null;
//            if (cacheUserObj instanceof JSONObject userJon)
            // 2021/10/23 直接转会报错 java.lang.IllegalArgumentException: argument type mismatch 。暂使用其他方式
//                jwtUserDto = userJon.toJavaObject(JwtUserDto.class);
//                jwtUserDto = JSONObject.parseObject(userJon.toJSONString(), JwtUserDto.class);
            if (cacheUserObj instanceof JwtUserDto jwtUser)
                jwtUserDto = jwtUser;
            else return null;
            // 更新权限信息
            jwtUserDto.getAuthorities();

            var userInner = jwtUserDto.getUser();
            // 检查dataScope是否修改
            List<Long> dataScopes = jwtUserDto.getDataScopes();
            dataScopes.clear();
            dataScopes.addAll(dataService.getDeptIds(userInner.getId(), userInner.getDept().getId()));
            searchDb = false;
        }
        if (searchDb) {
            UserInnerDto user;
            try {
                user = userService.findInnerUserByName(username);
            } catch (EntityNotFoundException e) {
                // SpringSecurity会自动转换UsernameNotFoundException为BadCredentialsException
                throw new UsernameNotFoundException("", e);
            }
            if (ObjectUtil.isNull(user.getId())) {
                throw new UsernameNotFoundException("");
            } else {
                if (Boolean.FALSE.equals(user.getEnabled())) {
                    throw new BadRequestException("账号未激活！");
                }
                // 2021/9/15 这里到authorities 序列化后，反序列化时，会有误。已初步解决
                jwtUserDto = new JwtUserDto(
                        user,
                        dataService.getDeptIds(user.getId(), user.getDept().getId())
                );
//                userDtoCache.put(username, jwtUserDto);
                // 设置用户信息有效期，6小时。理论上不设置也可以
                redisUtils.hPut(SecuritySysUtil.getUserCacheKey(), username, jwtUserDto, 6 * 60 * 60);
            }
        }
        return jwtUserDto;
    }
}
