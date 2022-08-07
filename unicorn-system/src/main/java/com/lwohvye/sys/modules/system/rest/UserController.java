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

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.api.modules.system.service.dto.UserInnerDto;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.config.RsaProperties;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.annotation.log.Log;
import com.lwohvye.api.modules.system.api.SysUserAPI;
import com.lwohvye.api.modules.system.domain.Dept;
import com.lwohvye.api.modules.system.domain.User;
import com.lwohvye.api.modules.system.domain.vo.UserBaseVo;
import com.lwohvye.api.modules.system.domain.vo.UserPassVo;
import com.lwohvye.sys.modules.system.service.IDataService;
import com.lwohvye.sys.modules.system.service.IDeptService;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.sys.modules.system.service.IUserService;
import com.lwohvye.api.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.api.modules.system.service.dto.UserQueryCriteria;
import com.lwohvye.utils.PageUtil;
import com.lwohvye.utils.RsaUtils;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.SpringContextHolder;
import com.lwohvye.utils.enums.CodeEnum;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 */
@Tag(name = "UserController", description = "系统：用户管理")
@Slf4j
@RestController
@RequiredArgsConstructor
public class UserController implements SysUserAPI {

    private final PasswordEncoder passwordEncoder;
    private final IUserService userService;
    private final IDataService dataService;
    private final IDeptService deptService;
    private final IRoleService roleService;

    @Operation(summary = "导出用户数据")
    @GetMapping(value = "/api/sys/users/download")
    public void download(HttpServletResponse response, UserQueryCriteria criteria) throws IOException {
        userService.download(userService.queryAll(criteria), response);
    }

    // 关于@InitBinder，有时间可以试一下，@InitBinder属于Controller级别的SpringMVC属性编辑器（只对所在的Controller生效）,并不是全局级别

    @Operation(summary = "查询用户")
    @Override
    public ResponseEntity<ResultInfo<Map<String, Object>>> query(UserQueryCriteria criteria, Pageable pageable) {
        if (!ObjectUtils.isEmpty(criteria.getDeptId())) {
            criteria.getDeptIds().add(criteria.getDeptId());
            // 先查找是否存在子节点
            List<Dept> data = deptService.findByPid(criteria.getDeptId());
            // 然后把子节点的ID都加入到集合中
            criteria.getDeptIds().addAll(deptService.getDeptChildren(data));
        }
        // 数据权限
        var curUser = userService.findByName(SecurityUtils.getCurrentUsername());
        List<Long> dataScopes = dataService.getDeptIds(curUser.getId(), curUser.getDept().getId());
        // criteria.getDeptIds() 不为空并且数据权限不为空则取交集
        if (!CollectionUtils.isEmpty(criteria.getDeptIds()) && !CollectionUtils.isEmpty(dataScopes)) {
            // 取交集
            criteria.getDeptIds().retainAll(dataScopes);
            if (!CollUtil.isEmpty(criteria.getDeptIds())) {
                return new ResponseEntity<>(ResultInfo.success(userService.queryAll(criteria, pageable)), HttpStatus.OK);
            }
        } else {
            // 否则取并集
            criteria.getDeptIds().addAll(dataScopes);
            return new ResponseEntity<>(ResultInfo.success(userService.queryAll(criteria, pageable)), HttpStatus.OK);
        }
        return new ResponseEntity<>(ResultInfo.success(PageUtil.toPage(null, 0)), HttpStatus.OK);
    }

    @Log("新增用户")
    @Operation(summary = "新增用户")
    @Override
    public ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody User resources) {
        checkLevel(resources);
        // 默认密码 123456
        resources.setPassword(passwordEncoder.encode("123456"));
        userService.create(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.CREATED);
    }

    @Log("修改用户")
    @Operation(summary = "修改用户")
    @Override
    public ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody User resources) throws Exception {
        checkLevel(resources);
        userService.update(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Operation(summary = "修改用户状态")
    @Override
    public ResponseEntity<ResultInfo<String>> updateStatus(@RequestBody UserBaseVo userVo) {
        userService.updateEnabled(userVo.getUsername(), userVo.getEnabled());
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("修改用户：个人中心")
    @Operation(summary = "修改用户：个人中心")
    @Override
    public ResponseEntity<ResultInfo<String>> center(@Validated(Update.class) @RequestBody User resources) {
        if (!resources.getId().equals(SecurityUtils.getCurrentUserId())) {
            throw new BadRequestException("不能修改他人资料");
        }
        userService.updateCenter(resources);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

    @Log("删除用户")
    @Operation(summary = "删除用户")
    @Override
    public ResponseEntity<ResultInfo<String>> delete(@RequestBody Set<Long> ids) {
        for (Long id : ids) {
            Integer currentLevel = roleService.findByUserId(SecurityUtils.getCurrentUserId()).stream().map(RoleSmallDto::getLevel).min(Integer::compareTo).orElseThrow();
            Integer optLevel = roleService.findByUserId(id).stream().map(RoleSmallDto::getLevel).min(Integer::compareTo).orElseThrow();
            if (currentLevel > optLevel) {
                throw new BadRequestException("角色权限不足，不能删除：" + userService.findById(id).getUsername());
            }
        }
        userService.delete(ids);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @Operation(summary = "修改密码")
    @Override
    public ResponseEntity<ResultInfo<String>> updatePass(@RequestBody UserPassVo passVo) throws Exception {
        String oldPass = RsaUtils.decryptByPrivateKey(RsaProperties.privateKey, passVo.getOldPass());
        String newPass = RsaUtils.decryptByPrivateKey(RsaProperties.privateKey, passVo.getNewPass());
        var user = userService.findInnerUserByName(SecurityUtils.getCurrentUsername());
        if (!passwordEncoder.matches(oldPass, user.getPassword())) {
            throw new BadRequestException("修改失败，旧密码错误");
        }
        if (passwordEncoder.matches(newPass, user.getPassword())) {
            throw new BadRequestException("新密码不能与旧密码相同");
        }
        userService.updatePass(user.getUsername(), passwordEncoder.encode(newPass));
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Operation(summary = "修改头像")
    @Override
    public ResponseEntity<Map<String, String>> updateAvatar(@RequestParam MultipartFile avatar) {
        return new ResponseEntity<>(userService.updateAvatar(avatar), HttpStatus.OK);
    }

    @Log("修改邮箱")
    @Operation(summary = "修改邮箱")
    @Override
    public ResponseEntity<ResultInfo<String>> updateEmail(@PathVariable String code, @RequestBody User user) throws Exception {
        String password = RsaUtils.decryptByPrivateKey(RsaProperties.privateKey, user.getPassword());
        var userDto = userService.findInnerUserByName(SecurityUtils.getCurrentUsername());
        if (!passwordEncoder.matches(password, userDto.getPassword())) {
            throw new BadRequestException("密码错误");
        }
        var beanName = "verifyServiceImpl";
        Object verifyService = null;
        try {
            verifyService = SpringContextHolder.getBean(beanName);
        } catch (Exception ex) {
            log.error("获取 {} 异常，原因 {} ，请确认是否引入相关模块", beanName, ex.getMessage());
            throw new BadRequestException("系统错误，请联系客服");
        }
        ReflectUtil.invoke(verifyService, "validated", CodeEnum.EMAIL_RESET_EMAIL_CODE.getKey() + user.getEmail(), code);
        userService.updateEmail(userDto.getUsername(), user.getEmail());
        return new ResponseEntity<>(HttpStatus.OK);
    }

    public ResponseEntity<ResultInfo<UserInnerDto>> queryByName(@PathVariable String username) {
        return new ResponseEntity<>(ResultInfo.success(userService.findInnerUserByName(username)), HttpStatus.OK);
    }

    /**
     * 如果当前用户的角色级别低于创建用户的角色级别，则抛出权限不足的错误
     *
     * @param resources /
     */
    private void checkLevel(User resources) {
        Integer currentLevel = roleService.findByUserId(SecurityUtils.getCurrentUserId()).stream().map(RoleSmallDto::getLevel).min(Integer::compareTo).orElseThrow();
        Integer optLevel = roleService.findByRoles(resources.getRoles());
        if (currentLevel > optLevel) {
            throw new BadRequestException("角色权限不足");
        }
    }
}
