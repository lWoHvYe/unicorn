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
package com.lwohvye.sys.modules.system.rest;

import cn.hutool.core.collection.CollectionUtil;
import com.lwohvye.annotation.log.Log;
import com.lwohvye.api.modules.system.domain.vo.MenuVo;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.api.modules.system.api.SysMenuAPI;
import com.lwohvye.api.modules.system.domain.Menu;
import com.lwohvye.api.modules.system.service.IMenuService;
import com.lwohvye.api.modules.system.service.dto.MenuDto;
import com.lwohvye.api.modules.system.service.dto.MenuQueryCriteria;
import com.lwohvye.sys.modules.system.service.mapstruct.MenuMapper;
import com.lwohvye.utils.PageUtil;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 * @date 2018-12-03
 */

@RestController
@RequiredArgsConstructor
@Tag(name = "MenuController", description = "系统：菜单管理")
public class MenuController implements SysMenuAPI {

    private final IMenuService menuService;
    private final MenuMapper menuMapper;
    private static final String ENTITY_NAME = "menu";

    @Operation(summary = "导出菜单数据")
    @GetMapping(value = "/api/sys/menus/download")
    public void download(HttpServletResponse response, MenuQueryCriteria criteria) throws Exception {
        menuService.download(menuService.queryAll(criteria, false), response);
    }

    @Operation(summary = "获取前端所需菜单")
    @Override
    public ResponseEntity<List<MenuVo>> buildMenus() {
        // 在方法参数里用SecurityUtils.getCurrentUserId()在一些情况下会不走缓存
        var cuid = SecurityUtils.getCurrentUserId();
        return new ResponseEntity<>(menuService.buildWebMenus(cuid), HttpStatus.OK);
    }

    @Operation(summary = "返回全部的菜单")
    @Override
    public ResponseEntity<List<MenuDto>> query(@RequestParam Long pid) {
        return new ResponseEntity<>(menuService.getMenus(pid), HttpStatus.OK);
    }

    @Operation(summary = "根据菜单ID返回所有子节点ID，包含自身ID")
    @Override
    public ResponseEntity<Set<Long>> child(@RequestParam Long id) {
        Set<Menu> menuSet = new HashSet<>();
        List<MenuDto> menuList = menuService.getMenus(id);
        menuSet.add(menuService.findOne(id));
        menuSet = menuService.getChildMenus(menuMapper.toEntity(menuList, new CycleAvoidingMappingContext()), menuSet);
        Set<Long> ids = menuSet.stream().map(Menu::getId).collect(Collectors.toSet());
        return new ResponseEntity<>(ids, HttpStatus.OK);
    }

    @Operation(summary = "查询菜单")
    @Override
    public ResponseEntity<ResultInfo<Map<String, Object>>> query(MenuQueryCriteria criteria) throws Exception {
        List<MenuDto> menuDtoList = menuService.queryAll(criteria, true);
        return new ResponseEntity<>(ResultInfo.success(PageUtil.toPage(menuDtoList, menuDtoList.size())), HttpStatus.OK);
    }

    @Operation(summary = "查询菜单:根据ID获取同级与上级数据")
    @Override
    public ResponseEntity<List<MenuDto>> getSuperior(@RequestBody List<Long> ids) {
        Set<MenuDto> menuDtos = new LinkedHashSet<>();
        if (CollectionUtil.isNotEmpty(ids)) {
            for (Long id : ids) {
                MenuDto menuDto = menuService.findById(id);
                menuDtos.addAll(menuService.getSuperior(menuDto, new ArrayList<>()));
            }
            return new ResponseEntity<>(menuService.buildTree(new ArrayList<>(menuDtos)), HttpStatus.OK);
        }
        return new ResponseEntity<>(menuService.getMenus(null), HttpStatus.OK);
    }

    @Log("新增菜单")
    @Operation(summary = "新增菜单")
    @Override
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Menu resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        menuService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改菜单")
    @Operation(summary = "修改菜单")
    @Override
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Menu resources) {
        menuService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除菜单")
    @Operation(summary = "删除菜单")
    @Override
    public ResponseEntity<ResultInfo<String>> delete(@RequestBody Set<Long> ids) {
        Set<Menu> menuSet = new HashSet<>();
        for (Long id : ids) {
            List<MenuDto> menuList = menuService.getMenus(id);
            menuSet.add(menuService.findOne(id));
            menuSet = menuService.getChildMenus(menuMapper.toEntity(menuList, new CycleAvoidingMappingContext()), menuSet);
        }
        menuService.delete(menuSet);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
