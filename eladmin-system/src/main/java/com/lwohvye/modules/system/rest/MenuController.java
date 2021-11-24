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
package com.lwohvye.modules.system.rest;

import cn.hutool.core.collection.CollectionUtil;
import com.lwohvye.annotation.Log;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.modules.system.domain.Menu;
import com.lwohvye.modules.system.service.IMenuService;
import com.lwohvye.modules.system.service.dto.MenuDto;
import com.lwohvye.modules.system.service.dto.MenuQueryCriteria;
import com.lwohvye.modules.system.service.mapstruct.MenuMapper;
import com.lwohvye.utils.PageUtil;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
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
@RequestMapping("/api/menus")
public class MenuController {

    private final IMenuService menuService;
    private final MenuMapper menuMapper;
    private static final String ENTITY_NAME = "menu";

    @Operation(summary = "导出菜单数据")
    @GetMapping(value = "/download")
    @PreAuthorize("@el.check('menu:list')")
    public void download(HttpServletResponse response, MenuQueryCriteria criteria) throws Exception {
        menuService.download(menuService.queryAll(criteria, false), response);
    }

    @GetMapping(value = "/build")
    @Operation(summary = "获取前端所需菜单")
    public ResponseEntity<Object> buildMenus() {
        // TODO: 2021/11/24 反序列化报错
        // org.springframework.data.redis.serializer.SerializationException: Could not read JSON: Unexpected token (START_OBJECT), expected VALUE_STRING: need JSON String that contains type id (for subtype of java.lang.Object)
        //  at [Source: (byte[])"[{"@class":"com.lwohvye.modules.system.service.dto.MenuDto","createBy":null,"updateBy":null,"createTime":["java.sql.Timestamp",1545117089000],"updateTime":null,"id":1,"children":null,"type":0,"permission":null,"title":"系统管理","menuSort":1,"path":"system","component":null,"pid":null,"subCount":7,"iFrame":false,"cache":false,"hidden":false,"componentName":null,"icon":"system","iframe":false,"label":"系统管理","hasChildren":true,"leaf":false},{"@class":"com.lwohvye.modules.system.service"[truncated 18149 bytes]; line: 1, column: 2]; nested exception is com.fasterxml.jackson.databind.exc.MismatchedInputException: Unexpected token (START_OBJECT), expected VALUE_STRING: need JSON String that contains type id (for subtype of java.lang.Object)
        //  at [Source: (byte[])"[{"@class":"com.lwohvye.modules.system.service.dto.MenuDto","createBy":null,"updateBy":null,"createTime":["java.sql.Timestamp",1545117089000],"updateTime":null,"id":1,"children":null,"type":0,"permission":null,"title":"系统管理","menuSort":1,"path":"system","component":null,"pid":null,"subCount":7,"iFrame":false,"cache":false,"hidden":false,"componentName":null,"icon":"system","iframe":false,"label":"系统管理","hasChildren":true,"leaf":false},{"@class":"com.lwohvye.modules.system.service"[truncated 18149 bytes]; line: 1, column: 2]
        List<MenuDto> menuDtoList = menuService.findByUser(SecurityUtils.getCurrentUserId());
        List<MenuDto> menuDtos = menuService.buildTree(menuDtoList);
//        List<MenuDto> menuDtos = menuService.buildTree2(menuDtoList);
        return new ResponseEntity<>(menuService.buildMenus(menuDtos), HttpStatus.OK);
    }

    @Operation(summary = "返回全部的菜单")
    @GetMapping(value = "/lazy")
    @PreAuthorize("@el.check('menu:list','roles:list')")
    public ResponseEntity<Object> query(@RequestParam Long pid) {
        return new ResponseEntity<>(menuService.getMenus(pid), HttpStatus.OK);
    }

    @Operation(summary = "根据菜单ID返回所有子节点ID，包含自身ID")
    @GetMapping(value = "/child")
    @PreAuthorize("@el.check('menu:list','roles:list')")
    public ResponseEntity<Object> child(@RequestParam Long id) {
        Set<Menu> menuSet = new HashSet<>();
        List<MenuDto> menuList = menuService.getMenus(id);
        menuSet.add(menuService.findOne(id));
        menuSet = menuService.getChildMenus(menuMapper.toEntity(menuList, new CycleAvoidingMappingContext()), menuSet);
        Set<Long> ids = menuSet.stream().map(Menu::getId).collect(Collectors.toSet());
        return new ResponseEntity<>(ids, HttpStatus.OK);
    }

    @GetMapping
    @Operation(summary = "查询菜单")
    @PreAuthorize("@el.check('menu:list')")
    public ResponseEntity<Object> query(MenuQueryCriteria criteria) throws Exception {
        List<MenuDto> menuDtoList = menuService.queryAll(criteria, true);
        return new ResponseEntity<>(ResultInfo.success(PageUtil.toPage(menuDtoList, menuDtoList.size())), HttpStatus.OK);
    }

    @Operation(summary = "查询菜单:根据ID获取同级与上级数据")
    @PostMapping("/superior")
    @PreAuthorize("@el.check('menu:list')")
    public ResponseEntity<Object> getSuperior(@RequestBody List<Long> ids) {
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
    @PostMapping
    @PreAuthorize("@el.check('menu:add')")
    public ResponseEntity<Object> create(@Validated @RequestBody Menu resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        menuService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改菜单")
    @Operation(summary = "修改菜单")
    @PutMapping
    @PreAuthorize("@el.check('menu:edit')")
    public ResponseEntity<Object> update(@Validated(Update.class) @RequestBody Menu resources) {
        menuService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除菜单")
    @Operation(summary = "删除菜单")
    @DeleteMapping
    @PreAuthorize("@el.check('menu:del')")
    public ResponseEntity<Object> delete(@RequestBody Set<Long> ids) {
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
