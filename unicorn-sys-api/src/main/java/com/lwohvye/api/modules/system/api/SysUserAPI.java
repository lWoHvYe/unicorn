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
package com.lwohvye.api.modules.system.api;

import com.lwohvye.api.modules.system.service.dto.UserInnerDto;
import com.lwohvye.base.BaseEntity.Update;
import com.lwohvye.api.modules.system.domain.User;
import com.lwohvye.api.modules.system.domain.vo.UserBaseVo;
import com.lwohvye.api.modules.system.domain.vo.UserPassVo;
import com.lwohvye.api.modules.system.service.dto.UserQueryCriteria;
import com.lwohvye.utils.result.ResultInfo;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Map;
import java.util.Set;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
public interface SysUserAPI {

    @GetMapping("/api/sys/users")
    ResponseEntity<ResultInfo<Map<String, Object>>> query(UserQueryCriteria criteria, Pageable pageable);

    @PostMapping("/api/sys/users")
    ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody User resources);

    @PutMapping("/api/sys/users")
    ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody User resources) throws Exception;

    @PostMapping("/api/sys/users/updateStatus")
    ResponseEntity<ResultInfo<String>> updateStatus(@RequestBody UserBaseVo userVo);

    @PutMapping("/api/sys/users/center")
    ResponseEntity<ResultInfo<String>> center(@Validated(Update.class) @RequestBody User resources);

    @DeleteMapping("/api/sys/users")
    ResponseEntity<ResultInfo<String>> delete(@RequestBody Set<Long> ids);

    @PostMapping("/api/sys/users/updatePass")
    ResponseEntity<ResultInfo<String>> updatePass(@RequestBody UserPassVo passVo) throws Exception;

    @PostMapping("/api/sys/users/updateAvatar")
    ResponseEntity<Map<String, String>> updateAvatar(@RequestParam MultipartFile avatar);

    @PostMapping("/api/sys/users/updateEmail/{code}")
    ResponseEntity<ResultInfo<String>> updateEmail(@PathVariable String code, @RequestBody User user) throws Exception;

    @GetMapping("/api/sys/users/name/{username}")
    ResponseEntity<ResultInfo<UserInnerDto>> queryByName(@PathVariable String username);
}
