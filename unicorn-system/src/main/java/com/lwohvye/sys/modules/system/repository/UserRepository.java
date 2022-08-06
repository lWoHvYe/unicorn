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
import com.lwohvye.api.modules.system.domain.Job;
import com.lwohvye.api.modules.system.domain.Role;
import com.lwohvye.api.modules.system.domain.User;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.*;
import org.springframework.lang.Nullable;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2018-11-22
 */
public interface UserRepository extends JpaRepository<User, Long>, JpaSpecificationExecutor<User> {

    @Override
    @EntityGraph(value = "User-Details")
    List<User> findAll(Specification<User> spec);

    // https://docs.spring.io/spring-data/jpa/docs/2.5.6/reference/html/#projections.interfaces.open.bean-reference
    @EntityGraph(value = "User-Details")
    <T> T findByUsername(String username, Class<T> clazz);

    /**
     * 若配置了Graph图，则查询会变成查满足条件的所有。然后内存分页。故分页类查询不建议使用。下面两个只作为存档。
     * 但一些根据特定条件只过滤出很少记录的，可以考虑使用，比如这里的根据id、name等的查询
     * 这个感觉跟MyBatis中配置的多对一的association标签，一对多的collection标签差不多，感觉很美好，但内存分页是硬伤
     *
     * @param spec
     * @param pageable
     * @return org.springframework.data.domain.Page
     * @date 2021/11/9 10:05 下午
     */
    @Override
    @EntityGraph(value = "User-Details")
    Page<User> findAll(@Nullable Specification<User> spec, Pageable pageable);

    @Override
    @EntityGraph(attributePaths = {"roles", "jobs", "dept"})
    <S extends User> Page<S> findAll(Example<S> example, Pageable pageable);

    /**
     * 根据用户名查询
     *
     * @param username 用户名
     * @return /
     */
    User findByUsername(String username);

    /**
     * 根据邮箱查询
     *
     * @param email 邮箱
     * @return /
     */
    User findByEmail(String email);

    /**
     * 根据手机号查询
     *
     * @param phone 手机号
     * @return /
     */
    User findByPhone(String phone);

    /**
     * 修改密码
     *
     * @param username              用户名
     * @param pass                  密码
     * @param lastPasswordResetTime /
     */
    @Modifying
    @Query(value = "update sys_user set password = ?2 , pwd_reset_time = ?3 where username = ?1", nativeQuery = true)
    void updatePass(String username, String pass, Date lastPasswordResetTime);

    /**
     * 修改邮箱
     *
     * @param username 用户名
     * @param email    邮箱
     */
    @Modifying
    @Query(value = "update sys_user set email = ?2 where username = ?1", nativeQuery = true)
    void updateEmail(String username, String email);

    @Modifying
    @Query(value = "update sys_user set enabled = ?3 where username = ?1 and enabled = ?2", nativeQuery = true)
    void updateEnabled(String username, Boolean expectStatus, Boolean enabled);

    /**
     * 根据角色查询用户
     *
     * @param roleId /
     * @return /
     */
    @Query(value = "SELECT u.* FROM sys_user u, sys_users_roles r WHERE" +
                   " u.user_id = r.user_id AND r.role_id = ?1", nativeQuery = true)
    List<User> findByRoleId(Long roleId);

    /**
     * 根据角色中的部门查询
     *
     * @param deptId /
     * @return /
     */
    @Query(value = "SELECT u.* FROM sys_user u, sys_users_roles r, sys_roles_depts d WHERE " +
                   "u.user_id = r.user_id AND r.role_id = d.role_id AND d.dept_id = ?1 group by u.user_id", nativeQuery = true)
    List<User> findByRoleDeptId(Long deptId);

    /**
     * 根据菜单查询
     *
     * @param id 菜单ID
     * @return /
     */
    @Query(value = "SELECT u.* FROM sys_user u, sys_users_roles ur, sys_roles_menus rm WHERE\n" +
                   "u.user_id = ur.user_id AND ur.role_id = rm.role_id AND rm.menu_id = ?1 group by u.user_id", nativeQuery = true)
    List<User> findByMenuId(Long id);

    /**
     * 根据Id删除
     *
     * @param ids /
     */
    void deleteAllByIdIn(Set<Long> ids);

    /**
     * 根据岗位查询
     *
     * @param ids /
     * @return /
     */
    @Query(value = "SELECT count(1) FROM sys_user u, sys_users_jobs j WHERE u.user_id = j.user_id AND j.job_id IN ?1", nativeQuery = true)
    int countByJobs(Collection<Long> ids);

    Boolean existsByJobsIn(Collection<Job> jobs);

    /**
     * 根据部门查询
     *
     * @param deptIds /
     * @return /
     */
    @Query(value = "SELECT count(1) FROM sys_user u WHERE u.dept_id IN ?1", nativeQuery = true)
    int countByDepts(Set<Long> deptIds);

    /**
     * 优化：使用exists判断是否存在
     *
     * @param deptIds /
     * @return java.lang.Boolean
     * @date 2021/6/15 1:27 下午
     */
    // 这种有连接查询。但除了id还支持别的 select user0_.user_id as col_0_0_ from sys_user user0_ left outer join sys_dept dept1_ on user0_.dept_id=dept1_.dept_id where dept1_.dept_id in (17 , 2) limit 1
    Boolean existsByDept_IdIn(Collection<Long> deptIds);

    // 这种无连接查询。但只支持id      select user0_.user_id as col_0_0_ from sys_user user0_ where user0_.dept_id in (2 , 17) limit 1
    Boolean existsByDeptIn(Collection<Dept> depts);

    /**
     * 根据角色查询
     *
     * @param ids /
     * @return /
     */
    @Query(value = "SELECT count(1) FROM sys_user u, sys_users_roles r WHERE " +
                   "u.user_id = r.user_id AND r.role_id in ?1", nativeQuery = true)
    int countByRoles(Collection<Long> ids);

    Boolean existsByRolesIn(Collection<Role> roles);

}
