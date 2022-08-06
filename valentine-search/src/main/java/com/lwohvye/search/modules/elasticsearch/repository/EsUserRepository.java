/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.search.modules.elasticsearch.repository;

import com.lwohvye.search.modules.elasticsearch.domain.EsUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

import java.util.Optional;

public interface EsUserRepository extends ElasticsearchRepository<EsUser,String> {

    Page<EsUser> findByRoleNameAndSexOrderByUserNameDesc(String roleName, Integer sex, Pageable pageable);

    Optional<EsUser> readByUserName(String userName);

    boolean existsByUserName(String userName);

}
