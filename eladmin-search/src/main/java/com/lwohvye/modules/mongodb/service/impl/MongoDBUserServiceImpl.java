/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.modules.mongodb.service.impl;

import com.lwohvye.modules.mongodb.domain.MongoDBUser;
import com.lwohvye.modules.mongodb.repository.MongoDBUserRepository;
import com.lwohvye.modules.mongodb.service.IMongoDBUserService;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

/**
 * @author Hongyan Wang
 * @date 2021年04月17日 13:53
 */
@Service
public class MongoDBUserServiceImpl implements IMongoDBUserService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private MongoDBUserRepository mongoDBUserRepository;

    @Override
    public Object queryAll() {
        return mongoDBUserRepository.findAll();
    }

    @Override
    public void updateUsers() {
        mongoDBUserRepository.deleteAll();
//        Updates an existing document or inserts a new document, depending on its document parameter
//If the document does not contain an _id field, then the save() method calls the insert() method. During the operation, the mongo shell will create an ObjectId and assign it to the _id field.
        userRepository.findAll().parallelStream().forEach(user -> {
            var username = user.getUsername();
            var mongoDBUser = mongoDBUserRepository.findFirstByUserName(username).orElseGet(MongoDBUser::new);
            mongoDBUser.setId(user.getId().toString()).setUserName(username).setPassWord(user.getPassword())
                    .setRoleName(user.getRoles().stream().map(Role::getName).collect(Collectors.joining("_")));
            mongoDBUserRepository.save(mongoDBUser);
        });
    }
}
