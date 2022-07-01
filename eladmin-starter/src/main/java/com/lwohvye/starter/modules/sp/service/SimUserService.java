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

package com.lwohvye.starter.modules.sp.service;

import cn.hutool.core.util.RandomUtil;
import com.lwohvye.starter.modules.sp.repository.SimUserRepository;
import com.lwohvye.starter.modules.sp.service.dto.URMDto;
import com.lwohvye.utils.SpringContextHolder;
import com.querydsl.core.types.Predicate;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.Visitor;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

import static com.lwohvye.starter.modules.sp.domain.QSimMenu.simMenu;
import static com.lwohvye.starter.modules.sp.domain.QSimRole.simRole;
import static com.lwohvye.starter.modules.sp.domain.QSimUser.simUser;
import static com.lwohvye.starter.modules.sp.domain.QSimUserRole.simUserRole;


@Slf4j
@Service
public class SimUserService {

    @Autowired
    private JPAQueryFactory jpaQueryFactory;

    @Autowired
    private SimUserRepository simUserRepository;

    // @PostConstruct
    public void doInit() {
        SpringContextHolder.addCallBacks(() -> this.demo("abc"));
    }

    public void demo(String str) {
        if (RandomUtil.randomBoolean()) return; // 随机启动失败

        simUserRepository.findAll(new Predicate() {
            @Override
            public <R, C> @Nullable R accept(Visitor<R, C> visitor, @Nullable C c) {
                return null;
            }

            @Override
            public Class<? extends Boolean> getType() {
                return null;
            }

            @Override
            public Predicate not() {
                return null;
            }
        }).forEach(su -> {
            // 像这种先查主体，再设置进去，也是一种方式感觉。不再定义实体时配置实体间的关系，感觉是另一种玩法
            su.setSimRoles(jpaQueryFactory.select(simRole).from(simRole, simUserRole).where(simRole.id.eq(simUserRole.roleId).and(simUserRole.userId.eq(su.getId()))).fetch());
            log.warn(su.toString());
        });
        //------------
        log.warn("________________");
        // 如果查的主表和返回的对象一致，可以直接用selectFrom，否则可以使用select + from来更灵活的定义返回对象及查的主表
        // 通过Projections可以映射自定义对象，支持构造、field、Bean投影。针对需要特殊值或运算的，感觉可以使用构造的方式，在里面做逻辑，但需注意构造模式入参都是String或数值类，需要额外转换为需要的类型
        var query = jpaQueryFactory.select(Projections.bean(URMDto.class, simUser.username.as("userName"), simRole.name.as("roleName"), simMenu.title.as("menuTitle")))
                .from(simUser)
                // 下面这些join感觉灵活性很高，在entiy的定义之外。在主查询的场合更灵活，但针对增删改，就没JPA那么智能了
                .leftJoin(simUserRole).on(simUser.id.eq(simUserRole.userId))
                .leftJoin(simRole).on(simRole.id.eq(simUserRole.roleId))
                .leftJoin(simMenu).on(simMenu.roleId.eq(simRole.id));
        // 可以这样多个where，它们间是`and`的关系，这是实现动态条件的基础
        query.where(simMenu.title.like(str + "%"));
        query.where(simUser.enabled.isTrue());
        query.where(simRole.code.isNotNull().and(simRole.code.eq(str)));

        query.limit(10L).fetch().forEach(su -> log.warn(su.toString()));
        // select simuser0_.username as col_0_0_, simrole2_.name as col_1_0_, simmenu3_.title as col_2_0_
        // from sys_user simuser0_ left outer join sys_users_roles simuserrol1_ on (simuser0_.user_id=simuserrol1_.user_id)
        // left outer join sys_role simrole2_ on (simrole2_.role_id=simuserrol1_.role_id)
        // left outer join sys_menu simmenu3_ on (simmenu3_.role_id=simrole2_.role_id)
        // where (simmenu3_.title like 'abc%' escape '!') and simuser0_.enabled=1 and (simrole2_.code is not null) and simrole2_.code='admin'
        // limit 10
        // where 第一个条件中的 escape '!' 不清楚是什么🐢
        // 另外还支持分页，而动态排序需通过OrderSpecifier来定义
    }
}
