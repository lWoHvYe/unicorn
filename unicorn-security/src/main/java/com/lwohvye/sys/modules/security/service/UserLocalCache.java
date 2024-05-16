/*
 * Copyright 2019-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.lwohvye.sys.modules.security.service;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.lwohvye.api.modules.system.service.dto.UserInnerDto;
import com.lwohvye.beans.config.LocalPropertyConfig;
import com.lwohvye.sys.modules.rabbitmq.config.RabbitMQConfig;
import com.lwohvye.sys.modules.rabbitmq.service.RabbitMQProducerService;
import com.lwohvye.sys.modules.security.service.dto.JwtUserDto;
import com.lwohvye.sys.modules.system.service.IDataService;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.sys.modules.system.service.IUserService;
import com.lwohvye.core.utils.StringUtils;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

import jakarta.persistence.EntityNotFoundException;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * @author: liaojinlong, lWoHvYe
 * @date: 2020/6/11 18:01
 * @apiNote: 用于清理 用户登录信息缓存，为防止Spring循环依赖与安全考虑 ，单独构成工具类
 */
@Component
public class UserLocalCache {

    @Lazy // 循环依赖
    @Autowired
    private IUserService userService;

    @Lazy
    @Autowired
    private IRoleService roleService;

    @Lazy
    @Autowired
    private IDataService dataService;

    @Autowired
    private RabbitMQProducerService rabbitMQProducerService;

    /**
     * 用户信息缓存
     */
    //  这种本地缓存的方式，也是解决热Key的一种方案，可以减轻Redis的压力（多个Redis集群，单个Redis不再保存全量数据，分散）。针对失效、过期等，可接入RQ，进行相关事件通知。
    //  不能存redis中，使用fastjson时没什么问题。但使用jackson反序列化需要实体有空参构造。而SimpleGrantedAuthority无空参构造。解决方案是自定义一个Authority类继承GrantedAuthority即可，感谢大佬那边的思路
    //  需要注意，使用localCache时，返回的对象与cache中是同一个对象，
    LoadingCache<String, JwtUserDto> userLRUCache = Caffeine.newBuilder()
            .initialCapacity(16) // 合理设置初始容量
            .maximumSize(32) // 最大容量，当缓存数量达到或接近该最大值时，Cache将清除掉那些最近最少使用的缓存
            .expireAfterAccess(24L, TimeUnit.MINUTES) // 读写缓存后多久过期
            .refreshAfterWrite(12L, TimeUnit.MINUTES) // 写入多久后刷新，刷新是一个异步的过程。理论上对时效性不强的，可以将这个与expire配合使用。
            // 这个刷新是惰性的，访问时若满足刷新条件了才会刷新，另外若此时已满足过期的条件，会走过期的逻辑。这俩配合使用且刷新短于过期可以解决短暂过期导致的问题，刷新只阻塞当前线程，其他线程依旧获取之前的数据，过期逻辑阻塞所有请求线程的，且线程安全
            // .refreshAfterWrite(Duration.of(12L, ChronoUnit.MINUTES))
            // .refreshAfterWrite(Duration.ofMinutes(12L))
            // .expireAfterWrite() // 写缓存后多久过期
            // .weigher((Weigher<String, JwtUserDto>) (username, jwtUserDto) -> jwtUserDto.isEnabled() ? 1 : 0) // 基于权重的清除策略，weigher can not be combined with maximum size
            // .softValues() // 可把key设为weak的，value可为weak或soft的
            // .removalListener((key, user, cause) -> System.out.printf("Key %s was removed (%s)%n", key, cause)) // 当元素被移除（主动/被动时触发监听）
            .build(this::getUserDB); // 数据不存在时，会调用load方法来获取


    @NotNull
    protected JwtUserDto getUserDB(String username) {
        JwtUserDto jwtUserDto;
        UserInnerDto user;
        try {
            user = userService.findInnerUserByName(username);
        } catch (EntityNotFoundException e) {
            // SpringSecurity会自动转换UsernameNotFoundException为BadCredentialsException
            throw new UsernameNotFoundException("", e);
        }
        if (Objects.isNull(user.getId()))
            throw new UsernameNotFoundException("");
        if (Boolean.FALSE.equals(user.getEnabled())) // 理论上只有不存在和未激活两种情况
            throw new InternalAuthenticationServiceException("用户已锁定");

        jwtUserDto = new JwtUserDto(
                user,
                dataService.getDataScope(user.getId()),
                roleService.grantedAuthorityGenHandler(user.getId(), user.getIsAdmin())
        );
        return jwtUserDto;
    }

    /**
     * 清理特定用户缓存信息<br>
     * 用户信息变更时
     *
     * @param userName /
     * @param doSync   是否广播事件
     */
    public void cleanUserCache(String userName, Boolean doSync) {
        if (StringUtils.isNotEmpty(userName)) {
            if (Boolean.TRUE.equals(doSync)) { // 广播事件
                var amqpMsg = new AmqpMsgEntity().setMsgType("sp").setMsgData(userName).setExtraData("cleanUserCache").setOrigin(LocalPropertyConfig.ORIGIN);
                rabbitMQProducerService.sendSyncDelayMsgEntity(RabbitMQConfig.SP_SYNC_ROUTE_KEY, amqpMsg);
            }
            userLRUCache.invalidate(userName); // 清除单个key
        }
    }

    /**
     * 清理所有用户的缓存信息<br>
     * ,如发生角色授权信息变化，可以简便的全部失效缓存
     */
    public void cleanAll() {
        userLRUCache.invalidateAll(); // 清除所有缓存项
    }
}
