/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.lwohvye.search.modules.elasticsearch.rest;

import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.annotation.rest.AnonymousPatchMapping;
import com.lwohvye.search.modules.elasticsearch.service.IEsUserService;
import com.lwohvye.utils.result.ResultInfo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("/api/es/user")
@RequiredArgsConstructor
public class EsUserController {

    private final IEsUserService esUserService;

    @AnonymousGetMapping
    public ResponseEntity getAllEsUser() {
        return new ResponseEntity<>(ResultInfo.success(esUserService.queryAll()), HttpStatus.OK);
    }

    @AnonymousPatchMapping
    public ResponseEntity updateUsers() {
        esUserService.updateUsers();
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

}
