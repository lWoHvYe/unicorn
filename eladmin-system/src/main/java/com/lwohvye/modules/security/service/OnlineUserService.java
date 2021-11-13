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

import cn.hutool.core.util.StrUtil;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import com.lwohvye.modules.security.service.dto.JwtUserDto;
import com.lwohvye.modules.security.service.dto.OnlineUserDto;
import com.lwohvye.modules.security.utils.SecuritySysUtil;
import com.lwohvye.utils.EncryptUtils;
import com.lwohvye.utils.FileUtil;
import com.lwohvye.utils.PageUtil;
import com.lwohvye.utils.StringUtils;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

/**
 * @author Zheng Jie
 * @date 2019年10月26日21:56:27
 * @deprecated JWT无状态，移除在线用户相关
 */
@Deprecated(since = "2.6.18")
@Service
@Slf4j
@RequiredArgsConstructor
public class OnlineUserService {

    private final SecurityProperties properties;
    private final RedisUtils redisUtils;

    /**
     * 保存在线用户信息
     *
     * @param jwtUserDto /
     * @param token      /
     * @param request    /
     */
    public void save(JwtUserDto jwtUserDto, String token, HttpServletRequest request) {
        String dept = jwtUserDto.getUser().getDept().getName();
        String ip = StringUtils.getIp(request);
        String browser = StringUtils.getBrowser(request);
        String address = StringUtils.getCityInfo(ip);
        OnlineUserDto onlineUserDto = null;
        try {
            onlineUserDto = new OnlineUserDto(jwtUserDto.getUsername(), jwtUserDto.getUser().getNickName(), dept, browser, ip, address, EncryptUtils.aesEncrypt(token), new Date());
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        redisUtils.set(SecuritySysUtil.getAuthToken(properties, token), onlineUserDto, properties.getTokenValidityInSeconds() / 1000);
    }

    /**
     * 查询全部数据
     *
     * @param filter   /
     * @param pageable /
     * @return /
     */
    public Map<String, Object> getAll(String filter, Pageable pageable) {
        List<OnlineUserDto> onlineUserDtos = getAll(filter);
        return PageUtil.toPage(
                PageUtil.toPage(pageable.getPageNumber(), pageable.getPageSize(), onlineUserDtos),
                onlineUserDtos.size()
        );
    }

    /**
     * 查询全部数据，不分页
     *
     * @param filter /
     * @return /
     */
    public List<OnlineUserDto> getAll(String filter) {
        // String类型的key被加上了双引号，导致此处无法模糊查询，
        // 双引号问题已解决，无法模糊是因为定义系统名称时加上了[]，这在正则里有特殊含义，所以不要用特殊字符
        List<String> keys = redisUtils.scan(SecuritySysUtil.getAuthToken(properties, "*"));
        Collections.reverse(keys);
        List<OnlineUserDto> onlineUserDtos = new ArrayList<>();
        for (String key : keys) {
            OnlineUserDto onlineUserDto = (OnlineUserDto) redisUtils.get(key);
            if (StringUtils.isNotBlank(filter)) {
                if (StrUtil.equals(onlineUserDto.getUserName(), filter)) {
                    onlineUserDtos.add(onlineUserDto);
                }
            } else {
                onlineUserDtos.add(onlineUserDto);
            }
        }
        onlineUserDtos.sort((o1, o2) -> o2.getLoginTime().compareTo(o1.getLoginTime()));
        return onlineUserDtos;
    }

    /**
     * 踢出用户
     *
     * @param key /
     */
    public void kickOut(String key) {
        key = SecuritySysUtil.getAuthToken(properties, key);
        redisUtils.delete(key);
    }

    /**
     * 退出登录
     *
     * @param token /
     */
    public void logout(String token) {
        String key = SecuritySysUtil.getAuthToken(properties, token);
        redisUtils.delete(key);
    }

    /**
     * 导出
     *
     * @param all      /
     * @param response /
     * @throws IOException /
     */
    public void download(List<OnlineUserDto> all, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (OnlineUserDto user : all) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("用户名", user.getUserName());
            map.put("部门", user.getDept());
            map.put("登录IP", user.getIp());
            map.put("登录地点", user.getAddress());
            map.put("浏览器", user.getBrowser());
            map.put("登录日期", user.getLoginTime());
            list.add(map);
        }
        FileUtil.downloadExcel(list, response);
    }

    /**
     * 根据token从缓存中获取用户
     *
     * @param authToken /
     * @return /
     */
    public OnlineUserDto getUserByToken(String authToken) {
        // 使用fastjson自带的FastJsonRedisSerializer时，从redis中取出的是JSON（JSONObject、JSONArray）对象
        var userObj = redisUtils.get(authToken);
        if (Objects.isNull(userObj))
            return null;
        // 先转成JSONObject，再转成onlineUser
//        if (userObj instanceof JSONObject userJSONObj)
//            return userJSONObj.toJavaObject(OnlineUserDto.class);
        if (userObj instanceof OnlineUserDto onlineUser)
            return onlineUser;
        return null;
    }

    /**
     * 检测用户是否在之前已经登录，已经登录踢下线
     *
     * @param userName 用户名
     */
    public void checkLoginOnUser(String userName, String ignoreToken) {
        List<OnlineUserDto> onlineUserDtos = getAll(userName);
        if (onlineUserDtos == null || onlineUserDtos.isEmpty()) {
            return;
        }
        for (OnlineUserDto onlineUserDto : onlineUserDtos) {
            try {
                String token = EncryptUtils.aesDecrypt(onlineUserDto.getKey());
                if (StringUtils.isBlank(ignoreToken) || !ignoreToken.equals(token))
                    this.kickOut(token);
            } catch (Exception e) {
                log.error("checkUser is error", e);
            }
        }
    }

    /**
     * 根据用户名强退用户
     *
     * @param username /
     */
    @Async
    public void kickOutForUsername(String username) {
        List<OnlineUserDto> onlineUsers = getAll(username);
        for (OnlineUserDto onlineUser : onlineUsers) {
            if (onlineUser.getUserName().equals(username)) {
                String token = EncryptUtils.aesDecrypt(onlineUser.getKey());
                kickOut(token);
            }
        }
    }
}
