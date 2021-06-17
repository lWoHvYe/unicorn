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

/**
 * @author: liaojinlong
 * @date: 2020/6/11 15:49
 * @apiNote: 关于缓存的Key集合
 */
public interface CacheKey {

    /**
     * 用户
     */
    String USER_ID = "user::" + LocalCoreConfig.SYS_NAME + "id:";
    /**
     * 数据
     */
    String DATA_USER = "data::" + LocalCoreConfig.SYS_NAME + "user:";
    /**
     * 菜单
     */
    String MENU_ID = "menu::" + LocalCoreConfig.SYS_NAME + "id:";
    String MENU_USER = "menu::" + LocalCoreConfig.SYS_NAME + "user:";
    /**
     * 角色授权
     */
    String ROLE_AUTH = "role::" + LocalCoreConfig.SYS_NAME + "auth:";
    /**
     * 角色信息
     */
    String ROLE_ID = "role::" + LocalCoreConfig.SYS_NAME + "id:";
    /**
     * 部门
     */
    String DEPT_ID = "dept::" + LocalCoreConfig.SYS_NAME + "id:";
    /**
     * 岗位
     */
    String JOB_ID = "job::" + LocalCoreConfig.SYS_NAME + "id:";
    /**
     * 数据字典
     */
    String DICT_NAME = "dict::" + LocalCoreConfig.SYS_NAME + "name:";
}
