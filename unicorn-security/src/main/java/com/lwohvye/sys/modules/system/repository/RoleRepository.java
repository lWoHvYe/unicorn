/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.sys.modules.system.repository;

import com.lwohvye.api.modules.system.domain.Dept;
import com.lwohvye.api.modules.system.domain.Role;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.*;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-12-03
 */
public interface RoleRepository extends JpaRepository<Role, Long>, JpaSpecificationExecutor<Role> {

    /**
     * 重写查询所有的方法，指定EntityGraph。查询就从多条查询变成了一条关联查询。但分页会变成内存分页。实际业务中，数据量大时不建议使用
     * 在多对多的关系中，若存在关联关系，但另一方已经不在了，在总查询后，还会有对不存在的那条的单查询（根据id查实体）。这时就会报错不存在。因此需要把关联表维护好，不要出现脏数据
     */
    // An entity graph can be used as a fetch or a load graph.
    // If a fetch graph is used, only the attributes specified by the entity graph will be treated as FetchType.EAGER. All other attributes will be lazy.
    // If a load graph is used, all attributes that are not specified by the entity graph will keep their default fetch type.
    //@Override
    // FETCH和LOAD这这里没有区别好像。这个会影响分页，有分页的不要用这个。并且这个跟查询注解一起用可能也有啥问题
    //@EntityGraph(value = "Role-Details", type = EntityGraph.EntityGraphType.FETCH)
    //Page<Role> findAll(Specification<Role> spec, Pageable pageable);

    // https://docs.spring.io/spring-data/jpa/docs/2.5.6/reference/html/#jpa.entity-graph
    @Override
    @EntityGraph(attributePaths = {"menus", "depts"})
    List<Role> findAll(Sort sort);

    /**
     * 根据名称查询
     *
     * @param name /
     * @return /
     */
    Role findByName(String name);

    /**
     * 删除多个角色
     *
     * @param ids /
     */
    void deleteAllByIdIn(Set<Long> ids);

    /**
     * 根据用户ID查询
     *
     * @param userId 用户ID
     * @return /
     */
    @Query(value = "SELECT r.* FROM sys_role r, sys_users_roles u WHERE " +
            "r.role_id = u.role_id AND u.user_id = ?1", nativeQuery = true)
    List<Role> findByUserId(Long userId);

    /**
     * 解绑角色菜单
     *
     * @param id 菜单ID
     */
    @Modifying
    @Query(value = "delete from sys_roles_menus where menu_id = ?1", nativeQuery = true)
    void untiedMenu(Long id);

    /**
     * 根据部门查询
     *
     * @param deptIds /
     * @return /
     */
    @Query(value = "select count(1) from sys_role r, sys_roles_depts d where " +
            "r.role_id = d.role_id and d.dept_id in ?1", nativeQuery = true)
    int countByDepts(Set<Long> deptIds);

    /**
     * 这里会多连一张表。虽然也只是通过dept.id筛选。这里主要验证in一个entity类型的变量
     *
     * @param depts
     * @return java.lang.Boolean
     * @date 2021/6/15 2:14 下午
     */
//    select role0_.role_id as col_0_0_ from sys_role role0_ left outer join sys_roles_depts depts1_
//    on role0_.role_id=depts1_.role_id left outer join sys_dept dept2_ on depts1_.dept_id=dept2_.dept_id
//    where dept2_.dept_id in (5) limit 1
    Boolean existsByDeptsIn(Collection<Set<Dept>> depts);

    /**
     * 根据菜单Id查询
     *
     * @param menuIds /
     * @return /
     */
    @Query(value = "SELECT r.* FROM sys_role r, sys_roles_menus m WHERE " +
            "r.role_id = m.role_id AND m.menu_id in ?1", nativeQuery = true)
    List<Role> findInMenuId(List<Long> menuIds);
}
