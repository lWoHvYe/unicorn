package com.lwohvye.modules.mongodb.repository;

import com.lwohvye.modules.mongodb.domain.MongoDBUser;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

/**
 * @author Hongyan Wang
 * @date 2021年04月17日 13:49
 */

public interface MongoDBUserRepository extends MongoRepository<MongoDBUser, String> {

    Optional<MongoDBUser> findFirstByUserName(String userName);

}
