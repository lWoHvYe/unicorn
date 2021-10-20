package com.lwohvye.modules.elasticsearch.repository;

import com.lwohvye.modules.elasticsearch.domain.EsUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

import java.util.Optional;

public interface EsUserRepository extends ElasticsearchRepository<EsUser,String> {

    Page<EsUser> findByRoleNameAndSexOrderByUserNameDesc(String roleName, Integer sex, Pageable pageable);

    Optional<EsUser> readByUserName(String userName);

    boolean existsByUserName(String userName);

}
