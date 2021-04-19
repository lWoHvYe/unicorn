package com.lwohvye.modules.system.service.impl;

import com.lwohvye.modules.system.domain.MongoDBUser;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.repository.MongoDBUserRepository;
import com.lwohvye.modules.system.repository.UserRepository;
import com.lwohvye.modules.system.service.MongoDBUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

/**
 * @author Hongyan Wang
 * @date 2021年04月17日 13:53
 */
@Service
@RequiredArgsConstructor
public class MongoDBUserServiceImpl implements MongoDBUserService {

    private final MongoDBUserRepository mongoDBUserRepository;

    private final UserRepository userRepository;

    @Override
    public Object findAll() {
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
