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

import cn.hutool.core.lang.Dict;
import com.lwohvye.log.annotation.Log;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.service.IRoleService;
import com.lwohvye.modules.system.service.dto.RoleDto;
import com.lwohvye.modules.system.service.dto.RoleQueryCriteria;
import com.lwohvye.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 * @date 2018-12-03
 */
@RestController
@RequiredArgsConstructor
@Tag(name = "RoleController", description = "系统：角色管理")
@RequestMapping("/api/sys/roles")
public class RoleController {

    private final IRoleService roleService;

    private static final String ENTITY_NAME = "role";

    @Operation(summary = "获取单个role")
    @GetMapping(value = "/{id}")
    public ResponseEntity<Object> query(@PathVariable Long id) {
        return new ResponseEntity<>(ResultInfo.success(roleService.findById(id)), HttpStatus.OK);
    }

    @Operation(summary = "导出角色数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, RoleQueryCriteria criteria) throws IOException {
        roleService.download(roleService.queryAll(criteria), response);
    }

    @Operation(summary = "返回全部的角色")
    @GetMapping(value = "/all")
    public ResponseEntity<Object> query() {
        return new ResponseEntity<>(ResultInfo.success(roleService.queryAll()), HttpStatus.OK);
    }

    @Operation(summary = "查询角色")
    @GetMapping
    public ResponseEntity<Object> query(RoleQueryCriteria criteria, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(roleService.queryAll(criteria, pageable)), HttpStatus.OK);
    }

    @Operation(summary = "获取用户级别")
    @GetMapping(value = "/level")
    public ResponseEntity<Object> getLevel() {
        return new ResponseEntity<>(ResultInfo.success(Dict.create().set("level", getLevels(null))), HttpStatus.OK);
    }

    @Log("新增角色")
    @Operation(summary = "新增角色")
    @PostMapping
    public ResponseEntity<Object> create(@Validated @RequestBody Role resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        getLevels(resources.getLevel());
        roleService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改角色")
    @Operation(summary = "修改角色")
    @PutMapping
    public ResponseEntity<Object> update(@Validated(Update.class) @RequestBody Role resources) {
        getLevels(resources.getLevel());
        roleService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("修改角色菜单")
    @Operation(summary = "修改角色菜单")
    @PutMapping(value = "/menu")
    public ResponseEntity<Object> updateMenu(@RequestBody Role resources) {
        RoleDto role = roleService.findById(resources.getId());
        getLevels(role.getLevel());
        roleService.updateMenu(resources, role);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除角色")
    @Operation(summary = "删除角色")
    @DeleteMapping
    public ResponseEntity<Object> delete(@RequestBody Set<Long> ids) {
        for (Long id : ids) {
            RoleDto role = roleService.findById(id);
            getLevels(role.getLevel());
        }
        // 验证是否被用户关联
        roleService.verification(ids);
        roleService.delete(ids);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    /**
     * 获取用户的角色级别
     *
     * @return /
     */
    private int getLevels(Integer level) {
        List<Integer> levels = roleService.findByUsersId(SecurityUtils.getCurrentUserId()).stream().map(RoleSmallDto::getLevel).collect(Collectors.toList());
        int min = Collections.min(levels);
        if (level != null) {
            if (level < min) {
                throw new BadRequestException("权限不足，你的角色级别：" + min + "，低于操作的角色级别：" + level);
            }
        }
        return min;
    }
}
