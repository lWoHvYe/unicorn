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
//    使用SPI机制的优势就是接口与实现的解耦，但是它也有部分限制。通过ServiceLoader延迟加载实现算是实现了延迟加载，
//    但是接口的实现的实例化只能通过无参函数构建。而对于存在多种实现时，我们只能全部遍历一遍所有实现造成了资源的浪费，并且想要获取指定的实现也不太灵活。
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
