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
