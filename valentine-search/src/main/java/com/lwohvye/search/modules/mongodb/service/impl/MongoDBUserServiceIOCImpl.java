/*
 *    Copyright (c) 2021-2023.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.search.modules.mongodb.service.impl;

import com.google.auto.service.AutoService;
import com.lwohvye.sys.modules.system.service.IUserService;
import com.lwohvye.api.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.api.modules.system.service.dto.UserQueryCriteria;
import com.lwohvye.search.modules.mongodb.domain.MongoDBUser;
import com.lwohvye.search.modules.mongodb.repository.MongoDBUserRepository;
import com.lwohvye.search.modules.mongodb.service.IMongoDBUserService;
import com.lwohvye.core.utils.SpringContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

@Slf4j
@Service
@AutoService(IMongoDBUserService.class)
public class MongoDBUserServiceIOCImpl implements IMongoDBUserService {
    // TODO: 2021/4/20 无论使用构造还是Autowired。注入都无值。当前通过在容器初始化后，调用doInit()来进行注入
//    使用SPI机制的优势就是接口与实现的解耦，但是它也有部分限制。通过ServiceLoader延迟加载实现算是实现了延迟加载，
//    但是接口的实现的实例化只能通过无参函数构建。而对于存在多种实现时，我们只能全部遍历一遍所有实现造成了资源的浪费，并且想要获取指定的实现也不太灵活。
    @Autowired
    private IUserService userService;

    @Autowired
    private MongoDBUserRepository mongoDBUserRepository;

    /**
     * SPI只能调用无参构造，所以要用这种方式来为属性赋值
     *
     * @date 2021/7/18 15:50
     */
//    @PostConstruct
    @Override
    public void doInit() {
        // 不能使用@PostConstruct，因为在执行该注解注释的方法时，ApplicationContext还未能获取到
        log.warn("进入  +++ {}", this.getClass().getSimpleName());
        this.userService = SpringContextHolder.getBean(IUserService.class);
        this.mongoDBUserRepository = SpringContextHolder.getBean(MongoDBUserRepository.class);
    }


    @Override
    public Object queryAll() {
        log.info("进入IOC实现");
        return mongoDBUserRepository.findAll();
    }

    @Override
    public void updateUsers() {
        log.info("进入IOC实现");
//        mongoDBUserRepository.deleteAll();
//        Updates an existing document or inserts a new document, depending on its document parameter
//If the document does not contain an _id field, then the save() method calls the insert() method. During the operation, the mongo shell will create an ObjectId and assign it to the _id field.
        userService.queryAll(new UserQueryCriteria()).parallelStream().forEach(user -> {
            var username = user.getUsername();
            var mongoDBUser = mongoDBUserRepository.findFirstByUserName(username).orElseGet(MongoDBUser::new);
            mongoDBUser.setId(user.getId().toString()).setUserName(username).setPassWord(user.getPassword())
                    .setRoleName(user.getRoles().stream().map(RoleSmallDto::getName).collect(Collectors.joining("_")));
            mongoDBUserRepository.save(mongoDBUser);
        });
    }
}
