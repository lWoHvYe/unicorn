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

package com.lwohvye.starter.modules.sp.repository;

import com.lwohvye.starter.modules.sp.domain.SimUser;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;

// 要使用Querydsl在创建repository时，一定要继承QueryDslPredicateExecutor接口，这种PredicateExecutor，也是一种动态条件的方式
public interface SimUserRepository extends JpaRepository<SimUser, Long>, QuerydslPredicateExecutor<SimUser> {
}
