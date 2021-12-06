/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.modules.mongodb.rest;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.RandomUtil;
import cn.hutool.core.util.ReflectUtil;
import com.lwohvye.modules.mongodb.service.IMongoDBUserService;
import com.lwohvye.utils.result.ResultInfo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.support.SpringFactoriesLoader;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ServiceLoader;

/**
 * @author Hongyan Wang
 * @date 2021年04月18日 22:16
 */
@Slf4j
@RestController
@RequestMapping("/api/mongodb/user")
public class MongoDBUserController {

    private IMongoDBUserService mongoDBUserService;

    /**
     * 启动成功后调用，执行部分初始化
     *
     * @date 2021/7/18 18:48
     */
    //    @PostConstruct
    private void doInit() {
        log.warn("进入Controller层方法：{}", this.getClass().getSimpleName());
        if (ObjectUtil.isNull(mongoDBUserService)) {
//            SPI的两种方式：Java ServiceLoader和Spring SpringFactoriesLoader。当前都无法注入属性。待解决
            if (RandomUtil.randomBoolean()) {
                var mongoDBUserServices = ServiceLoader.load(IMongoDBUserService.class);
//        有值，且大小唯一
                if (ObjectUtil.isNotNull(mongoDBUserServices) && CollUtil.isNotEmpty(mongoDBUserServices) && ObjectUtil.equal(mongoDBUserServices.stream().count(), 1L)) {
                    var optionalMongoDBUserService = mongoDBUserServices.findFirst();
                    optionalMongoDBUserService.ifPresent(dbUserService -> this.mongoDBUserService = dbUserService);
                }
            } else {
                var mongoDBUserServices = SpringFactoriesLoader.loadFactories(IMongoDBUserService.class, null);
//        有值，且大小唯一
                if (ObjectUtil.isNotNull(mongoDBUserServices) && CollUtil.isNotEmpty(mongoDBUserServices) && ObjectUtil.equal(mongoDBUserServices.size(), 1)) {
                    this.mongoDBUserService = mongoDBUserServices.get(0);
                }
            }
            // 对SPI注入的实体做初始化，部分属性填充，也算是一种方式
            ReflectUtil.invoke(this.mongoDBUserService, "doInit");
        }
    }

    @GetMapping
    public ResponseEntity getAllUser() {
        return new ResponseEntity<>(ResultInfo.success(mongoDBUserService.queryAll()), HttpStatus.OK);
    }

    @PostMapping
    public ResponseEntity updateUsers() {
        mongoDBUserService.updateUsers();
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }
}
