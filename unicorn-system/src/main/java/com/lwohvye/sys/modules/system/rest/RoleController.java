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

import cn.hutool.core.lang.Dict;
import com.lwohvye.core.annotation.ResponseResultBody;
import com.lwohvye.core.annotation.log.Log;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.api.modules.system.api.SysRoleAPI;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.api.modules.system.service.dto.RoleDto;
import com.lwohvye.api.modules.system.service.dto.RoleQueryCriteria;
import com.lwohvye.api.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.core.utils.SecurityUtils;
import com.lwohvye.core.utils.result.ResultInfo;
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
import java.util.Map;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-12-03
 */
@Tag(name = "RoleController", description = "系统：角色管理")
@RestController
@ResponseResultBody
@RequiredArgsConstructor
public class RoleController implements SysRoleAPI {

    private final IRoleService roleService;

    private static final String ENTITY_NAME = "role";

    @Operation(summary = "获取单个role")
    @Override
    public RoleDto query(@PathVariable Long id) {
        return roleService.findById(id);
    }

    @Operation(summary = "导出角色数据")
    @GetMapping(value = "/api/sys/roles/download")
    public void download(HttpServletResponse response, RoleQueryCriteria criteria) throws IOException {
        roleService.download(roleService.queryAll(criteria), response);
    }

    @Operation(summary = "返回全部的角色")
    @Override
    public List<RoleDto> query() {
        return roleService.queryAll();
    }

    @Operation(summary = "查询角色")
    @Override
    public Map<String, Object> query(RoleQueryCriteria criteria, Pageable pageable) {
        return roleService.queryAll(criteria, pageable);
    }

    @Operation(summary = "获取用户级别")
    @Override
    public Dict getLevel() {
        return Dict.create().set("level", getLevels(null));
    }

    @Log("新增角色")
    @Operation(summary = "新增角色")
    @Override
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody Role resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        getLevels(resources.getLevel());
        roleService.create(resources);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Log("修改角色")
    @Operation(summary = "修改角色")
    @Override
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody Role resources) {
        getLevels(resources.getLevel());
        roleService.update(resources);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Log("修改角色菜单")
    @Operation(summary = "修改角色菜单")
    @Override
    public ResponseEntity<ResultInfo<String>> updateMenu(@RequestBody Role resources) {
        RoleDto role = roleService.findById(resources.getId());
        getLevels(role.getLevel());
        roleService.updateMenu(resources, role);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Log("删除角色")
    @Operation(summary = "删除角色")
    @Override
    public ResultInfo<String> delete(@RequestBody Set<Long> ids) {
        for (Long id : ids) {
            RoleDto role = roleService.findById(id);
            getLevels(role.getLevel());
        }
        // 验证是否被用户关联
        roleService.verification(ids);
        roleService.delete(ids);
        return ResultInfo.success();
    }

    public List<RoleSmallDto> queryByUid(@PathVariable Long userId) {
        return roleService.findByUserId(userId);
    }

    /**
     * 获取用户的角色级别
     *
     * @return /
     */
    private int getLevels(Integer level) {
        List<Integer> levels = roleService.findByUserId(SecurityUtils.getCurrentUserId()).stream().map(RoleSmallDto::getLevel).toList();
        int min = Collections.min(levels);
        if (level != null && level < min) {
            throw new BadRequestException("权限不足，你的角色级别：" + min + "，低于操作的角色级别：" + level);
        }
        return min;
    }
}
