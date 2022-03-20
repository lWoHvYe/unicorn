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
import com.lwohvye.annotation.log.Log;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.modules.system.api.SysDeptAPI;
import com.lwohvye.modules.system.domain.Dept;
import com.lwohvye.modules.system.service.IDataService;
import com.lwohvye.modules.system.service.IDeptService;
import com.lwohvye.modules.system.service.dto.DeptDto;
import com.lwohvye.modules.system.service.dto.DeptQueryCriteria;
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

/**
 * @author Zheng Jie
 * @date 2019-03-25
 */
@RestController
@RequiredArgsConstructor
@Tag(name = "DeptController", description = "系统：部门管理")
public class DeptController implements SysDeptAPI {

    private final IDeptService deptService;
    private final IDataService dataService;
    private static final String ENTITY_NAME = "dept";

    @Operation(summary = "导出部门数据")
    @GetMapping(value = "/download")
    public void download(HttpServletResponse response, DeptQueryCriteria criteria) throws Exception {
        deptService.download(deptService.queryAll(SecurityUtils.getCurrentUserId(), criteria, false), response);
    }

    @Operation(summary = "查询部门")
    @Override
    public ResponseEntity<Object> query(DeptQueryCriteria criteria) throws Exception {
        List<DeptDto> deptDtos = deptService.queryAll(SecurityUtils.getCurrentUserId(), criteria, true);
        return new ResponseEntity<>(ResultInfo.success(PageUtil.toPage(deptDtos, deptDtos.size())), HttpStatus.OK);
    }

    @Operation(summary = "查询部门:根据ID获取同级与上级数据")
    @Override
    public ResponseEntity<Object> getSuperior(@RequestBody List<Long> ids) {
        Set<DeptDto> deptDtos = new LinkedHashSet<>();
        for (Long id : ids) {
            DeptDto deptDto = deptService.findById(id);
            List<DeptDto> depts = deptService.getSuperior(deptDto, new ArrayList<>());
            deptDtos.addAll(depts);
        }
        return new ResponseEntity<>(ResultInfo.success(deptService.buildTree(new ArrayList<>(deptDtos))), HttpStatus.OK);
    }

    @Log("新增部门")
    @Operation(summary = "新增部门")
    @Override
    public ResponseEntity<Object> create(@Validated @RequestBody Dept resources) {
        if (resources.getId() != null) {
            throw new BadRequestException("A new " + ENTITY_NAME + " cannot already have an ID");
        }
        deptService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改部门")
    @Operation(summary = "修改部门")
    @Override
    public ResponseEntity<Object> update(@Validated(Update.class) @RequestBody Dept resources) {
        deptService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除部门")
    @Operation(summary = "删除部门")
    @Override
    public ResponseEntity<Object> delete(@RequestBody Set<Long> ids) {
        Set<DeptDto> deptDtos = new HashSet<>();
        for (Long id : ids) {
            List<Dept> deptList = deptService.findByPid(id);
            deptDtos.add(deptService.findById(id));
            if (CollectionUtil.isNotEmpty(deptList)) {
                deptDtos = deptService.getDeleteDepts(deptList, deptDtos);
            }
        }
        // 验证是否被角色或用户关联
        deptService.verification(deptDtos);
        deptService.delete(deptDtos);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    public ResponseEntity<Object> queryEnabledDeptIds(@PathVariable Long userId, @PathVariable Long deptId) {
        return new ResponseEntity<>(ResultInfo.success(dataService.getDeptIds(userId, deptId)), HttpStatus.OK);
    }
}
