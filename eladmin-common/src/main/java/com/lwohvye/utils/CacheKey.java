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
package com.lwohvye.utils;

import com.lwohvye.config.LocalCoreConfig;

import java.util.Map;

/**
 * @author: liaojinlong
 * @date: 2020/6/11 15:49
 * @apiNote: 关于缓存的Key集合
 */
public interface CacheKey {

    String CACHE_NAME = "cacheName";

    /**
     * 用户
     */
    // 接口中可以定义属性，默认（指不用加，但也不能改）是 public static final 的
    Map<String, String> USER_ID = Map.of(CACHE_NAME, "user", "key", LocalCoreConfig.SYS_NAME + "id:");
    /**
     * 数据
     */
    Map<String, String> DATA_USER = Map.of(CACHE_NAME, "data", "key", LocalCoreConfig.SYS_NAME + "user:");
    /**
     * 菜单
     */
    Map<String, String> MENU_ID = Map.of(CACHE_NAME, "menu", "key", LocalCoreConfig.SYS_NAME + "id:");

    Map<String, String> MENU_USER = Map.of(CACHE_NAME, "menu", "key", LocalCoreConfig.SYS_NAME + "menu4user:");
    /**
     * 角色信息
     */
    Map<String, String> ROLE_ID = Map.of(CACHE_NAME, "role", "key", LocalCoreConfig.SYS_NAME + "id:");
    /**
     * 部门
     */
    Map<String, String> DEPT_ID = Map.of(CACHE_NAME, "dept", "key", LocalCoreConfig.SYS_NAME + "id:");
}
