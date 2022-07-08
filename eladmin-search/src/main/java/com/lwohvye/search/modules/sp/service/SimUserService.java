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

package com.lwohvye.search.modules.sp.service;

import com.blazebit.persistence.CriteriaBuilderFactory;
import com.blazebit.persistence.querydsl.BlazeJPAQuery;
import com.blazebit.persistence.querydsl.JPQLNextExpressions;
import com.lwohvye.search.modules.sp.domain.SimUser;
import com.lwohvye.search.modules.sp.repository.SimUserRepository;
import com.lwohvye.search.modules.sp.service.dto.URMDto;
import com.lwohvye.utils.SpringContextHolder;
import com.querydsl.core.Tuple;
import com.querydsl.core.group.GroupBy;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.StringPath;
import com.querydsl.jpa.JPAExpressions;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.persistence.EntityManager;
import java.util.Collections;
import java.util.List;

import static com.lwohvye.search.modules.sp.domain.QIdHolderCte.idHolderCte;
import static com.lwohvye.search.modules.sp.domain.QSimMenu.simMenu;
import static com.lwohvye.search.modules.sp.domain.QSimRole.simRole;
import static com.lwohvye.search.modules.sp.domain.QSimUser.simUser;
import static com.lwohvye.search.modules.sp.domain.QSimUserRole.simUserRole;


@Slf4j
@Service
public class SimUserService {

    @Lazy
    @Autowired
    private SimUserService simUserService;

    @Autowired
    private JPAQueryFactory jpaQueryFactory;

    @Autowired
    private CriteriaBuilderFactory criteriaBuilderFactory;

    @Autowired
    private EntityManager entityManager;

    @Autowired
    private SimUserRepository simUserRepository;

    @PostConstruct
    public void doInit() {
        SpringContextHolder.addCallBacks(() -> simUserService.demo("ad"));
        SpringContextHolder.addCallBacks(simUserService::demo02);
    }

    @Transactional(rollbackFor = Exception.class)
    public void demo(String str) {

        // simUserRepository.findAll(new Predicate() {
        //     @Override
        //     public <R, C> @Nullable R accept(Visitor<R, C> visitor, @Nullable C c) {
        //         return null;
        //     }
        //
        //     @Override
        //     public Class<? extends Boolean> getType() {
        //         return null;
        //     }
        //
        //     @Override
        //     public Predicate not() {
        //         return null;
        //     }
        // }).forEach(su -> {
        //     // 像这种先查主体，再设置进去，也是一种方式感觉。不再定义实体时配置实体间的关系，感觉是另一种玩法
        //     su.setSimRoles(jpaQueryFactory.select(simRole).from(simRole, simUserRole).where(simRole.id.eq(simUserRole.roleId).and(simUserRole.userId.eq(su.getId()))).fetch());
        //     log.warn(su.toString());
        // });
        //------------
        log.warn("________________");
        // 如果查的主表和返回的对象一致，可以直接用selectFrom，否则可以使用select + from来更灵活的定义返回对象及查的主表
        // 通过Projections可以映射自定义对象，支持构造、field、Bean投影。针对需要特殊值或运算的，感觉可以使用构造的方式，在里面做逻辑，但需注意构造模式入参都是String或数值类，需要额外转换为需要的类型
        // var query = jpaQueryFactory.select(Projections.bean(URMDto.class, simUser.username.as("userName"), simRole.name.as("roleName"), simMenu.title.as("menuTitle")))
        //         .from(simUser)
        // 下面这些join感觉灵活性很高，在entiy的定义之外。在主查询的场合更灵活，但针对增删改，就没JPA那么智能了
        // .leftJoin(simUserRole).on(simUser.id.eq(simUserRole.userId))
        // .leftJoin(simRole).on(simRole.id.eq(simUserRole.roleId))
        // .leftJoin(simMenu).on(simMenu.roleId.eq(simRole.id));
        // 可以这样多个where，它们间是`and`的关系，这是实现动态条件的基础
        // query.where(simMenu.title.like(str + "%"));
        // query.where(simUser.enabled.isTrue());
        // query.where(simRole.code.isNotNull().and(simRole.code.eq(str)));

        // var res01 = query.limit(10L).fetch();
        // res01.forEach(su -> log.warn(su.toString()));
        // select simuser0_.username as col_0_0_, simrole2_.name as col_1_0_, simmenu3_.title as col_2_0_
        // from sys_user simuser0_ left outer join sys_users_roles simuserrol1_ on (simuser0_.user_id=simuserrol1_.user_id)
        // left outer join sys_role simrole2_ on (simrole2_.role_id=simuserrol1_.role_id)
        // left outer join sys_menu simmenu3_ on (simmenu3_.role_id=simrole2_.role_id)
        // where (simmenu3_.title like 'abc%' escape '!') and simuser0_.enabled=1 and (simrole2_.code is not null) and simrole2_.code='admin'
        // limit 10
        // where 第一个条件中的 escape '!' 不清楚是什么🐢,escape '/' 是指用'/'说明在/后面的字符不是通配符，而是普通符，这里的 ! 应该类似吧

        // 另外还支持分页（基于offset和limit无法获取到totalCount），而动态排序需通过OrderSpecifier来定义
        // this approach only works for simple queries. Specifically queries with `multiple group by clauses` and queries with a `having clause` turn out to be problematic.
        // 下面这种不推荐使用，Deprecated。官方推荐去使用Blaze-Persistence
        // var queryResults01 = query.fetchResults();
        // var res02 = queryResults01.getResults();
        // var totalCount01 = queryResults01.getTotal();
        // var count = query.fetchCount();

        // Among other advanced query features, Blaze-Persistence makes it possible to select from subqueries in JPQL.
        var blazeJPAQuery = new BlazeJPAQuery<URMDto>(entityManager, criteriaBuilderFactory);
        blazeJPAQuery.select(Projections.bean(URMDto.class, simUser.username.as("userName"), simRole.name.as("roleName"), simMenu.title.as("menuTitle")))
                .from(simUser)
                // 下面这些join感觉灵活性很高，在entiy的定义之外。在主查询的场合更灵活，但针对增删改，就没JPA那么智能了
                .leftJoin(simUserRole).on(simUser.id.eq(simUserRole.userId))
                .leftJoin(simRole).on(simRole.id.eq(simUserRole.roleId))
                .leftJoin(simMenu).on(simMenu.simRole.eq(simRole));
        // blazeJPAQuery.where(simMenu.title.like(str + "%"));
        blazeJPAQuery.where(simUser.enabled.isTrue());
        blazeJPAQuery.where(simRole.code.isNotNull().and(simRole.code.like(str + "%")));

        blazeJPAQuery.orderBy(simUser.username.desc());// order by case when simuser0_.username is null then 1 else 0 end, simuser0_.username ASC 针对String当做asc时，会有这种优化
        // 获取总记录数，若放到jpqlQuery.offset(10).limit(5);之后，会报 No transactional EntityManager available
        var totalCount02 = blazeJPAQuery.fetchCount();
        blazeJPAQuery.offset(10).limit(5); // 分页信息
        var res04 = blazeJPAQuery.fetch();
        res04.forEach(su -> log.warn(su.toString()));
        // var pageRes01 = blazeJPAQuery.fetchPage(1, 10); //这种方式会报错，The order by items of the query builder are not guaranteed to produce unique tuples! Consider also ordering by the entity identifier!
        // var totalSize = pageRes01.getTotalSize();

        // To create a subquery you use the static factory methods of JPAExpressions and define the query parameters via from, where etc.
        jpaQueryFactory.selectFrom(simUser).where(simUser.id.in(
                JPAExpressions.select(simUserRole.userId).from(simRole, simUserRole).where(simRole.id.eq(simUserRole.roleId), simRole.name.eq(str)))
        ).fetch().forEach(su -> log.warn(su.toString()));
        // 看了MyBatis Dynamic SQL的Doc后，感觉很不错，并提供了不错的扩展点，后续有机会打算试一下
    }

    @Transactional(rollbackFor = Exception.class)
    public void demo02() {
        //  Simple query
        //  select simuser0_.username as col_0_0_, substring(simuser0_.username, 3) as col_1_0_ from sys_user_view simuser0_ where length(simuser0_.username)>4
        var query = new BlazeJPAQuery<Tuple>(entityManager, criteriaBuilderFactory).from(simUser)
                .select(simUser.username.as("name"), simUser.username.substring(2))
                .where(simUser.username.length().gt(4));

        List<Tuple> fetch = query.fetch();

        // Regular association joins 下面这两个算是两种写法吧，都是聚合，但有循环依赖问题
        // select simrole0_.role_id as role_id1_21_0_, simmenus1_.menu_id as menu_id1_16_1_, simrole0_.code
        // as code2_21_0_, simrole0_.description as descript3_21_0_, simrole0_.name as name4_21_0_, simmenus1_.name
        // as name2_16_1_, simmenus1_.pid as pid3_16_1_, simmenus1_.role_id as role_id6_16_1_, simmenus1_.title
        // as title4_16_1_, simmenus1_.type as type5_16_1_
        // from sys_role_view simrole0_ inner join sys_menu_view simmenus1_ on simrole0_.role_id=simmenus1_.role_id
        var booksByAuthor = new BlazeJPAQuery<>(entityManager, criteriaBuilderFactory)
                .from(simRole)
                .innerJoin(simRole.simMenus, simMenu)
                .transform(GroupBy.groupBy(simRole).as(GroupBy.list(simMenu)));

        // Regular entity joins
        // select simrole0_.role_id as role_id1_21_0_, simmenu1_.menu_id as menu_id1_16_1_, simrole0_.code
        // as code2_21_0_, simrole0_.description as descript3_21_0_, simrole0_.name as name4_21_0_, simmenu1_.name
        // as name2_16_1_, simmenu1_.pid as pid3_16_1_, simmenu1_.role_id as role_id6_16_1_, simmenu1_.title
        // as title4_16_1_, simmenu1_.type as type5_16_1_
        // from sys_role_view simrole0_ inner join sys_menu_view simmenu1_ on (simmenu1_.role_id=simrole0_.role_id)
        var booksByAuthor02 = new BlazeJPAQuery<>(entityManager, criteriaBuilderFactory)
                .from(simRole)
                .innerJoin(simMenu).on(simMenu.simRole.eq(simRole))
                .transform(GroupBy.groupBy(simRole).as(GroupBy.list(simMenu)));

        //  Managed type value clause
        // Add a VALUES clause for values of the given value class to the from clause. This introduces a parameter named like the given alias.
        // select simuser0_.user_id as user_id1_25_, simuser0_.enabled as enabled2_25_, simuser0_.password
        // as password3_25_, simuser0_.phone as phone4_25_, simuser0_.username as username5_25_
        // from (select * from
        //          (select null as enabled,null as user_id,null as password,null as phone,null as username from dual
        //          union all
        //          select NULL,NULL,'tpw',NULL,'tun' from dual ) fltr_nulls_tbl_als_
        //          where fltr_nulls_tbl_als_.enabled is not null or fltr_nulls_tbl_als_.user_id is not null or fltr_nulls_tbl_als_.password is not null or fltr_nulls_tbl_als_.phone is not null or fltr_nulls_tbl_als_.username is not null
        //       ) simuser0_
        var theUser = new SimUser();
        theUser.setUsername("tun");
        theUser.setPassword("tpw");
        var fetch02 = new BlazeJPAQuery<>(entityManager, criteriaBuilderFactory)
                .fromValues(simUser, Collections.singleton(theUser))
                .select(simUser)
                .fetch();

        // Managed attribute value clause
        // Add a VALUES clause for values of the type as determined by the given entity attribute to the from clause. This introduces a parameter named like the given alias.
        // select simuser0_.username as col_0_0_ from (select * from (select null as username from dual union all select 'Fluffy' from dual ) fltr_nulls_tbl_als_
        // where fltr_nulls_tbl_als_.username is not null) simuser0_
        StringPath catName = Expressions.stringPath("catName");
        var fetch03 = new BlazeJPAQuery<>(entityManager, criteriaBuilderFactory)
                .fromValues(simUser.username, catName, Collections.singleton("Fluffy"))
                .select(catName)
                .fetch();

        // Window functions。这个后续看一下
        // Window functions are available through the various static utility methods in JPQLNextExpressions. For convenience, its recommended to add a star-import to com.blazebit.persistence.querydsl.JPQLNextExpressions.*.
        // select simuser0_.username as col_0_0_, ROW_NUMBER() OVER () as col_1_0_, LAST_VALUE(simuser0_.username) OVER (PARTITION BY simuser0_.user_id) as col_2_0_ from sys_user_view simuser0_
        var query05 = new BlazeJPAQuery<Tuple>(entityManager, criteriaBuilderFactory).from(simUser)
                .select(simUser.username, JPQLNextExpressions.rowNumber(), JPQLNextExpressions.lastValue(simUser.username).over().partitionBy(simUser.id));
        List<Tuple> fetch05 = query05.fetch();

        // Named window functions
        // NamedWindow myWindow = new NamedWindow("myWindow").partitionBy(simUser.id);
        // BlazeJPAQuery<Tuple> query06 = new BlazeJPAQuery<Tuple>(entityManager, criteriaBuilderFactory).from(simUser)
        //         .select(simUser.username, JPQLNextExpressions.rowNumber().over(myWindow), JPQLNextExpressions.lastValue(simUser.username).over(myWindow));
        // List<Tuple> fetch06 = query06.fetch();

        // Common Table Expressions 这个也是个高级的用法，还没搞明白
        // select idholderct0_.id as col_0_0_ from
        //  (select null id,null name from dual where 1=0
        //  union all
        //  (select simmenu0_.menu_id as col_0_0_, simmenu0_.title as col_1_0_ from sys_menu_view simmenu0_)
        //  ) idholderct0_
        List<Long> fetch07 = new BlazeJPAQuery<>(entityManager, criteriaBuilderFactory)
                .with(idHolderCte, JPQLNextExpressions.select(
                        JPQLNextExpressions.bind(idHolderCte.id, simMenu.id),
                        JPQLNextExpressions.bind(idHolderCte.name, simMenu.title)).from(simMenu))
                .select(idHolderCte.id).from(idHolderCte)
                .fetch();
        //     List<Long> fetch08 = new BlazeJPAQuery<>(entityManager, criteriaBuilderFactory)
        //             .with(idHolderCte, new BlazeJPAQuery()
        //                     .bind(idHolderCte.id, simMenu.id),
        //             .bind(idHolderCte.name, simMenu.title)).from(simMenu))
        // .select(idHolderCte.id).from(idHolderCte)
        //             .fetch();

    }
}
