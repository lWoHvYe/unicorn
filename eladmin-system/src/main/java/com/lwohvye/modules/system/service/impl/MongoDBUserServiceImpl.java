package com.lwohvye.modules.system.service.impl;

import com.lwohvye.modules.system.repository.MongoDBUserRepository;
import com.lwohvye.modules.system.service.MongoDBUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

/**
 * @author Hongyan Wang
 * @date 2021年04月17日 13:53
 */
@Service
@RequiredArgsConstructor
public class MongoDBUserServiceImpl implements MongoDBUserService {
    private final MongoDBUserRepository mongoDBUserRepository;

}
