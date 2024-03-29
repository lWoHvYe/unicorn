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
package com.lwohvye.sys.modules.system.service;

import com.lwohvye.api.modules.system.domain.vo.MenuVo;
import com.lwohvye.core.base.BaseService;
import com.lwohvye.api.modules.system.domain.Menu;
import com.lwohvye.api.modules.system.service.dto.MenuDto;
import com.lwohvye.api.modules.system.service.dto.MenuQueryCriteria;

import com.lwohvye.core.base.SimplePOJO;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-12-17
 */
public interface IMenuService extends BaseService {

    /**
     * 查询全部数据
     *
     * @param criteria 条件
     * @param isQuery  /
     * @return /
     * @throws Exception /
     */
    List<MenuDto> queryAll(MenuQueryCriteria criteria, Boolean isQuery) throws Exception;

    /**
     * 根据ID查询
     *
     * @param id /
     * @return /
     */
    MenuDto findById(long id);

    /**
     * 创建
     *
     * @param resources /
     */
    void create(Menu resources);

    /**
     * 编辑
     *
     * @param resources /
     */
    void update(Menu resources);

    /**
     * 获取所有子节点，包含自身ID
     */
    List<SimplePOJO> fetchMenuByPid(Long pid);

    /**
     * 构建菜单树
     *
     * @param menuDtos 原始数据
     * @return /
     */
    List<MenuDto> buildTree(List<MenuDto> menuDtos);

    List<MenuDto> buildTree2(List<MenuDto> menuDtos);

    List<MenuDto> buildTree3(Long currentUserId);

    /**
     * 构建菜单树
     *
     * @param menuDtos /
     * @return /
     */
    List<MenuVo> buildMenus(List<MenuDto> menuDtos);

    List<MenuVo> buildWebMenus(Long uid);

    /**
     * 根据ID查询
     *
     * @param id /
     * @return /
     */
    Menu findOne(Long id);

    /**
     * 删除
     *
     * @param menuSet /
     */
    void batchDelete(Set<MenuDto> menuSet);

    /**
     * 导出
     *
     * @param queryAll 待导出的数据
     * @param response /
     * @throws IOException /
     */
    void download(List<MenuDto> queryAll, HttpServletResponse response) throws IOException;

    /**
     * 懒加载菜单数据
     *
     * @param pid /
     * @return /
     */
    List<MenuDto> fetchRootOrTargetMenus(Long pid);

    /**
     * 根据ID获取同级与上级数据
     *
     * @param menuDto /
     * @param objects /
     * @return /
     */
    List<MenuDto> getSuperior(MenuDto menuDto, List<Menu> objects);

    /**
     * 根据当前用户获取菜单
     *
     * @param currentUserId /
     * @return /
     */
    List<MenuDto> findByUser(Long currentUserId);
}
