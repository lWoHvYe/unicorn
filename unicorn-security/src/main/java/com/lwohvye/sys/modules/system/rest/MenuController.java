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
import com.lwohvye.api.modules.system.api.SysMenuAPI;
import com.lwohvye.api.modules.system.domain.Menu;
import com.lwohvye.api.modules.system.domain.vo.MenuVo;
import com.lwohvye.api.modules.system.service.dto.MenuDto;
import com.lwohvye.api.modules.system.service.dto.MenuQueryCriteria;
import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.core.base.SimplePOJO;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.PageUtils;
import com.lwohvye.core.utils.SecurityUtils;
import com.lwohvye.core.utils.result.ResultInfo;
import com.lwohvye.sys.modules.system.service.IMenuService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 * @date 2018-12-03
 */

@Tag(name = "MenuController", description = "系统：菜单管理")
@RestController
@RespResultBody
@RequiredArgsConstructor
public class MenuController implements SysMenuAPI {

    private final IMenuService menuService;
    private static final String ENTITY_NAME = "menu";

    @Operation(summary = "导出菜单数据")
    @GetMapping(value = "/api/sys/menus/download")
    public void download(HttpServletResponse response, MenuQueryCriteria criteria) throws Exception {
        menuService.download(menuService.queryAll(criteria, false), response);
    }

    @Operation(summary = "获取前端所需菜单")
    @Override
    public List<MenuVo> buildMenus() {
        // 在方法参数里用SecurityUtils.getCurrentUserId()在一些情况下会不走缓存
        var cuid = SecurityUtils.getCurrentUserId();
        return menuService.buildWebMenus(cuid);
    }

    @Operation(summary = "返回全部的菜单")
    @Override
    public List<MenuDto> query(@RequestParam Long pid) {
        return menuService.fetchRootOrTargetMenus(pid);
    }

    @Operation(summary = "根据菜单ID返回所有子节点ID，包含自身ID")
    @Override
    public List<Long> child(@RequestParam Long id) {
        var simpleMenus = menuService.fetchMenuByPid(id);
        return simpleMenus.stream().map(SimplePOJO::id).distinct().collect(Collectors.toList());
    }

    @Operation(summary = "查询菜单")
    @Override
    public Map<String, Object> query(MenuQueryCriteria criteria) throws Exception {
        List<MenuDto> menuDtoList = menuService.queryAll(criteria, true);
        return PageUtils.toPage(menuDtoList, menuDtoList.size());
    }

    @Operation(summary = "查询菜单:根据ID获取同级与上级数据")
    @Override
    public List<MenuDto> getSuperior(@RequestBody List<Long> ids) {
        Set<MenuDto> menuDtos = new LinkedHashSet<>();
        if (CollectionUtil.isNotEmpty(ids)) {
            for (Long id : ids) {
                MenuDto menuDto = menuService.findById(id);
                menuDtos.addAll(menuService.getSuperior(menuDto, new ArrayList<>()));
            }
            return menuService.buildTree(new ArrayList<>(menuDtos));
        }
        return menuService.fetchRootOrTargetMenus(null);
    }

    @OprLog("新增菜单")
    @Operation(summary = "新增菜单")
    @Override
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Menu resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        menuService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @OprLog("修改菜单")
    @Operation(summary = "修改菜单")
    @Override
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Menu resources) {
        menuService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @OprLog("删除菜单")
    @Operation(summary = "删除菜单")
    @Override
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        var menuSet = Optional.of(ids).orElseGet(Collections::emptySet)
                .stream().flatMap(id -> menuService.fetchMenuByPid(id).stream())
                .distinct()
                .map(simplePOJO -> {
                    var menuDto = new MenuDto();
                    menuDto.setId(simplePOJO.id());
                    menuDto.setTitle(simplePOJO.name());
                    menuDto.setPid(simplePOJO.pid());
                    return menuDto;
                }).collect(Collectors.toSet());
        menuService.batchDelete(menuSet);
        return ResultInfo.success();
    }
}
