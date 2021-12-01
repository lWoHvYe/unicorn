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

package com.lwohvye.modules.elasticsearch.service.impl;

import cn.hutool.core.util.RandomUtil;
import com.lwohvye.modules.elasticsearch.domain.EsUser;
import com.lwohvye.modules.elasticsearch.repository.EsUserRepository;
import com.lwohvye.modules.elasticsearch.service.IEsUserService;
import com.lwohvye.modules.mongodb.repository.MongoDBUserRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class EsUserServiceImpl implements IEsUserService {

    @Autowired
    private EsUserRepository esUserRepository;

    @Autowired
    private MongoDBUserRepository mongoDBUserRepository;

    @Override
    public Object queryAll() {
        return esUserRepository.findAll();
    }

    @Override
    public void updateUsers() {
//        esUserRepository.deleteAll();
        mongoDBUserRepository.findAll().parallelStream().forEach(mongoDBUser -> {
            var userName = mongoDBUser.getUserName();
            // 查询
            var esUser = esUserRepository.readByUserName(userName).orElseGet(EsUser::new);
            // 赋值
            esUser.setId(mongoDBUser.getId()).setUserName(mongoDBUser.getUserName()).setPassWord(mongoDBUser.getPassWord())
                    .setRoleName(mongoDBUser.getRoleName()).setSex(RandomUtil.randomBoolean() ? 0 : 1);

            esUserRepository.save(esUser);
        });

    }
}
