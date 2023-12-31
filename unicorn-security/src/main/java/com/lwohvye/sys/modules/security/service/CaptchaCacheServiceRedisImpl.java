/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.modules.security.service;

import com.anji.captcha.service.CaptchaCacheService;
import com.lwohvye.core.base.BaseService;
import com.lwohvye.core.utils.SpringContextHolder;
import com.lwohvye.core.utils.redis.RedisUtils;

import java.util.Objects;

/**
 * 对于分布式部署的应用，我们建议应用自己实现CaptchaCacheService，比如用Redis，参考service/spring-boot代码示例。
 * 如果应用是单点的，也没有使用redis，那默认使用内存。
 * 内存缓存只适合单节点部署的应用，否则验证码生产与验证在节点之间信息不同步，导致失败。
 * <p>
 * ☆☆☆ SPI： 在resources目录新建META-INF.services文件夹(两层)，参考当前服务resources。Java 9之后通过在module-info.java中通过providers xx with xx来实现
 *
 * @author lide1202@hotmail.com
 * @Title: 使用redis缓存
 * @date 2020-05-12
 */
public class CaptchaCacheServiceRedisImpl implements CaptchaCacheService, BaseService {

    private RedisUtils redisUtils;

    public CaptchaCacheServiceRedisImpl() {
        SpringContextHolder.addCallBacks(this::doInit);
    }

    @Override
    public void doInit() {
        if (Objects.isNull(redisUtils)) redisUtils = SpringContextHolder.getBean(RedisUtils.class);
    }

    @Override
    public String type() {
        return "redis";
    }

    @Override
    public void set(String key, String value, long expiresInSeconds) {
        redisUtils.set(key, value, expiresInSeconds);
    }

    @Override
    public boolean exists(String key) {
        return redisUtils.hasKey(key);
    }

    @Override
    public void delete(String key) {
        redisUtils.delete(key);
    }

    @Override
    public String get(String key) {
        return (String) redisUtils.get(key);
    }

    @Override
    public Long increment(String key, long val) {
        return redisUtils.incrBy(key, val);
    }
}
