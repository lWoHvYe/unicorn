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
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * @author Zheng Jie
 * @date 2019-03-25
 */
public interface DeptRepository extends JpaRepository<Dept, Long>, JpaSpecificationExecutor<Dept> {

    /**
     * 根据 PID 查询
     *
     * @param pid pid
     * @return /
     */
    List<Dept> findByPid(Long pid);

    Optional<List<Dept>> findByPidIn(Collection<Long> pids);

    /**
     * 获取顶级部门
     *
     * @return /
     */
    List<Dept> findByPidIsNull();

    /**
     * 判断是否存在子节点
     *
     * @param pid /
     * @return /
     */
    int countByPid(Long pid);

    /**
     * 根据ID更新sub_count
     *
     * @param count /
     * @param id    /
     */
    @Modifying
    @Query(value = " update sys_dept set sub_count = ?1 where dept_id = ?2 ", nativeQuery = true)
    void updateSubCntById(Integer count, Long id);
}
