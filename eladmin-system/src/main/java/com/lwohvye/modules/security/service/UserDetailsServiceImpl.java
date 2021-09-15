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
import com.alibaba.fastjson.JSONObject;
import com.lwohvye.config.redis.AuthRedisUtils;
import com.lwohvye.config.redis.AuthSlaveRedisUtils;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.exception.EntityNotFoundException;
import com.lwohvye.modules.security.config.bean.LoginProperties;
import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.modules.system.service.DataService;
import com.lwohvye.modules.system.service.RoleService;
import com.lwohvye.modules.system.service.UserService;
import com.lwohvye.modules.system.service.dto.UserInnerDto;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author Zheng Jie
 * @date 2018-11-22
 */
@Service("userDetailsService")
@RequiredArgsConstructor
public class UserDetailsServiceImpl implements UserDetailsService {
    private final UserService userService;
    private final RoleService roleService;
    private final DataService dataService;
    private final LoginProperties loginProperties;
    private final AuthRedisUtils authRedisUtils;
    private final AuthSlaveRedisUtils authSlaveRedisUtils;

    public void setEnableCache(boolean enableCache) {
        this.loginProperties.setCacheEnable(enableCache);
    }

    /**
     * 用户信息缓存
     *
     * @see {@link UserCacheClean}
     */
    //  这种缓存的方式，也许解决了些问题，但导致无法做集群的扩展，故调整为分布式缓存redis
//    static Map<String, JwtUserDto> userDtoCache = new ConcurrentHashMap<>();

    // 用户缓存的redis key
    public static final String USER_CACHE_KEY = "Sys-User-JwtInfo-Cache";

    @Override
    public JwtUserDto loadUserByUsername(String username) {
        boolean searchDb = true;
        JwtUserDto jwtUserDto = null;
        if (loginProperties.isCacheEnable() && authSlaveRedisUtils.hHasKey(USER_CACHE_KEY, username)) {
//            jwtUserDto = userDtoCache.get(username);
            var cacheUser = (String)authSlaveRedisUtils.hGet(USER_CACHE_KEY, username);
            jwtUserDto = JSONObject.parseObject(cacheUser, JwtUserDto.class);
            // 检查dataScope是否修改
            List<Long> dataScopes = jwtUserDto.getDataScopes();
            dataScopes.clear();
            dataScopes.addAll(dataService.getDeptIds(jwtUserDto.getUser()));
            searchDb = false;
        }
        if (searchDb) {
            UserInnerDto user;
            try {
                var dto = userService.findByName(username);
                user = new UserInnerDto(dto);
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
                jwtUserDto = new JwtUserDto(
                        user,
                        dataService.getDeptIds(user),
                        roleService.mapToGrantedAuthorities(user)
                );
//                userDtoCache.put(username, jwtUserDto);
                // 不能直接存对象，会报错。。。。com.alibaba.fastjson.JSONException: autoType is not support.
                // https://github.com/alibaba/fastjson/wiki/enable_autotype      但已经开启了，原因未知
                authRedisUtils.hPut(USER_CACHE_KEY, username, JSONObject.toJSONString(jwtUserDto));
            }
        }
        return jwtUserDto;
    }
}
