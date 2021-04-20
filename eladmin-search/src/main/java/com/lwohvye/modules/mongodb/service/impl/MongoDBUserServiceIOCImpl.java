package com.lwohvye.modules.mongodb.service.impl;

import com.lwohvye.modules.mongodb.domain.MongoDBUser;
import com.lwohvye.modules.mongodb.service.MongoDBUserService;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

@Service
public class MongoDBUserServiceIOCImpl implements MongoDBUserService {
    // TODO: 2021/4/20 无论使用构造还是Autowired。注入都无值
    @Autowired
    private UserRepository userRepository;

    @Override
    public Object findAll() {
        return userRepository.findAll().parallelStream().map(user -> {
            var username = user.getUsername();
            return new MongoDBUser().setId(user.getId().toString()).setUserName(username).setPassWord(user.getPassword())
                    .setRoleName(user.getRoles().stream().map(Role::getName).collect(Collectors.joining("_")));
        }).collect(Collectors.toList());
    }

    @Override
    public void updateUsers() {
        System.out.printf("更新方法");
    }
}
