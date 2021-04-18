package com.lwohvye.modules.system.repository;

import com.lwohvye.modules.system.domain.MongoDBUser;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * @author Hongyan Wang
 * @date 2021年04月17日 13:49
 */
@Repository
public interface MongoDBUserRepository extends MongoRepository<MongoDBUser, Long> {

    Optional<MongoDBUser> findFirstByUserName(String userName);

}
