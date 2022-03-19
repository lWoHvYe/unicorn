/*
 *  Copyright 2019-2022 lWoHvYe
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
package com.lwohvye.modules.system.api;

import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.modules.system.domain.User;
import com.lwohvye.modules.system.domain.vo.UserPassVo;
import com.lwohvye.modules.system.service.dto.UserQueryCriteria;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Set;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
@RequestMapping("/api/sys/users")
public interface SysUserAPI {

    @GetMapping
    ResponseEntity<Object> query(UserQueryCriteria criteria, Pageable pageable);

    @PostMapping
    ResponseEntity<Object> create(@Validated @RequestBody User resources);

    @PutMapping
    ResponseEntity<Object> update(@Validated(Update.class) @RequestBody User resources) throws Exception;

    @PutMapping(value = "center")
    ResponseEntity<Object> center(@Validated(Update.class) @RequestBody User resources);

    @DeleteMapping
    ResponseEntity<Object> delete(@RequestBody Set<Long> ids);

    @PostMapping(value = "/updatePass")
    ResponseEntity<Object> updatePass(@RequestBody UserPassVo passVo) throws Exception;

    @PostMapping(value = "/updateAvatar")
    ResponseEntity<Object> updateAvatar(@RequestParam MultipartFile avatar);

    @PostMapping(value = "/updateEmail/{code}")
    ResponseEntity<Object> updateEmail(@PathVariable String code, @RequestBody User user) throws Exception;

}
