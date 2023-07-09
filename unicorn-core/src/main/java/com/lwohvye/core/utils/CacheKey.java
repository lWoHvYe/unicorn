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
package com.lwohvye.core.utils;

import com.lwohvye.core.config.LocalCoreConfig;

import java.util.Map;

/**
 * @author: liaojinlong
 * @date: 2020/6/11 15:49
 * @apiNote: 关于缓存的Key集合，这里只保留一种方式，能在Interface中定义Constant，实际不建议使用
 */
public interface CacheKey {

    String CACHE_NAME = "cacheName"; // public static final String在编译时，会执行常量替换

    String CACHE_KEY = "key";
    /**
     * 用户
     */
    // 接口中可以定义属性，默认（指不用加，但也不能改）是 public static final 的
    Map<String, String> USER_ID = Map.of(CACHE_NAME, "user", CACHE_KEY, LocalCoreConfig.SYS_NAME + "id:");
    /**
     * 数据
     */
    Map<String, String> DATA_USER = Map.of(CACHE_NAME, "data", CACHE_KEY, LocalCoreConfig.SYS_NAME + "user:");
    Map<String, String> DATA_SCOPE = Map.of(CACHE_NAME, "data", CACHE_KEY, LocalCoreConfig.SYS_NAME + "data_scope4user:");

    /**
     * 菜单
     */
    Map<String, String> MENU_ID = Map.of(CACHE_NAME, "menu", CACHE_KEY, LocalCoreConfig.SYS_NAME + "id:");

    Map<String, String> MENU_USER = Map.of(CACHE_NAME, "menu", CACHE_KEY, LocalCoreConfig.SYS_NAME + "menu4user:");
    /**
     * 角色信息
     */
    Map<String, String> ROLE_ID = Map.of(CACHE_NAME, "role", CACHE_KEY, LocalCoreConfig.SYS_NAME + "id:");
    /**
     * 部门
     */
    Map<String, String> DEPT_ID = Map.of(CACHE_NAME, "dept", CACHE_KEY, LocalCoreConfig.SYS_NAME + "id:");

    Map<String, String> RESOURCE_ALL = Map.of(CACHE_NAME, "resource", CACHE_KEY, LocalCoreConfig.SYS_NAME + "allResources");
}
