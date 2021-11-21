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
package com.lwohvye.modules.security.rest;

import com.lwohvye.modules.security.service.OnlineUserService;
import com.lwohvye.utils.EncryptUtils;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Set;

/**
 * @author Zheng Jie
 * @deprecated JWT无状态，移除在线用户相关
 */
@Deprecated(since = "2.6.18")
@RestController
@RequiredArgsConstructor
@RequestMapping("/auth/online")
@Tag(name = "OnlineController", description = "系统：在线用户管理")
public class OnlineController {

    private final OnlineUserService onlineUserService;

    @Operation(summary = "查询在线用户")
    @GetMapping
    @PreAuthorize("@el.check()")
    public ResponseEntity<Object> query(String filter, Pageable pageable) {
        return new ResponseEntity<>(ResultInfo.success(onlineUserService.getAll(filter, pageable)), HttpStatus.OK);
    }

    @Operation(summary = "导出数据")
    @GetMapping(value = "/download")
    @PreAuthorize("@el.check()")
    public void download(HttpServletResponse response, String filter) throws IOException {
        onlineUserService.download(onlineUserService.getAll(filter), response);
    }

    @Operation(summary = "踢出用户")
    @DeleteMapping
    @PreAuthorize("@el.check()")
    public ResponseEntity<Object> delete(@RequestBody Set<String> keys) {
        for (String key : keys) {
            // 解密Key
            key = EncryptUtils.aesDecrypt(key);
            onlineUserService.kickOut(key);
        }
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }
}
