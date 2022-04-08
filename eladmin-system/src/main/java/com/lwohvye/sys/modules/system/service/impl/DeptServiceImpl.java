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
package com.lwohvye.sys.modules.system.service.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.api.modules.system.domain.Dept;
import com.lwohvye.sys.modules.system.repository.DeptRepository;
import com.lwohvye.sys.modules.system.service.IDeptService;
import com.lwohvye.sys.modules.system.service.IRoleService;
import com.lwohvye.sys.modules.system.service.IUserService;
import com.lwohvye.api.modules.system.service.dto.DeptDto;
import com.lwohvye.api.modules.system.service.dto.DeptQueryCriteria;
import com.lwohvye.sys.modules.system.service.mapstruct.DeptMapper;
import com.lwohvye.sys.modules.system.subject.DeptSubject;
import com.lwohvye.utils.*;
import com.lwohvye.utils.enums.DataScopeEnum;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

/**
 * @author Zheng Jie
 * @date 2019-03-25
 */
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "dept")
public class DeptServiceImpl extends DeptSubject implements IDeptService {

    private final DeptRepository deptRepository;
    private final DeptMapper deptMapper;
    private final IUserService userService;
    private final IRoleService roleService;
    private final RedisUtils redisUtils;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public List<DeptDto> queryAll(Long currentUserId, DeptQueryCriteria criteria, Boolean isQuery) throws Exception {
        Sort sort = Sort.by(Sort.Direction.ASC, "deptSort");
        String dataScopeType = SecurityUtils.getDataScopeType();
        // 下面这块的大致逻辑是，数据权限是全部的，先默认查一级节点。若除了pidIsNull和enabled外其他属性有值，就移除查一级的限制。已改为重写set方法
//        if (isQuery) {
        if (Boolean.TRUE.equals(isQuery) && !Objects.equals(dataScopeType, DataScopeEnum.ALL.getValue())) {
            // 非全部数据权限，移除只查一级节点，与DeptQueryCriteria中的默认值，和重写的set配合
            criteria.setPidIsNull(null);
//            }
//            List<Field> fields = QueryHelp.getAllFields(criteria.getClass(), new ArrayList<>());
//            List<String> fieldNames = new ArrayList<>() {
//                {
//                    add("pidIsNull");
//                    add("enabled");
//                }
//            };
//            for (Field field : fields) {
//                //设置对象的访问权限，保证对private的属性的访问
//                field.setAccessible(true);
//                Object val = field.get(criteria);
//                if (fieldNames.contains(field.getName())) {
//                    continue;
//                }
//                // 数据权限为全部
//                if (ObjectUtil.isNotNull(val)) {
//                    criteria.setPidIsNull(null);
//                    break;
//                }
//            }
        }
        List<DeptDto> list = deptMapper.toDto(deptRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), sort), new CycleAvoidingMappingContext());
        // 如果为空，就代表为自定义权限或者本级权限，就需要去重，不理解可以注释掉，看查询结果
        if (StringUtils.isBlank(dataScopeType)) {
            return deduplication(list);
        }
        return list;
    }

    @Override
    @Cacheable(key = " #root.target.getSysName() + 'id:' + #p0")
    @Transactional(rollbackFor = Exception.class)
    public DeptDto findById(Long id) {
        Dept dept = deptRepository.findById(id).orElseGet(Dept::new);
        ValidationUtil.isNull(dept.getId(), "Dept", "id", id);
        return deptMapper.toDto(dept, new CycleAvoidingMappingContext());
    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public List<Dept> findByPid(long pid) {
        return deptRepository.findByPid(pid);
    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public Set<Dept> findByRoleId(Long id) {
        return deptRepository.findByRoleId(id);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(Dept resources) {
        deptRepository.save(resources);
        // 计算子节点数目
        resources.setSubCount(0);
        // 更新节点数
        updateSubCnt(resources.getPid());
        // 清理自定义角色权限的datascope缓存
        delCaches(resources.getPid());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(Dept resources) {
        // 旧的部门
        Long oldPid = findById(resources.getId()).getPid();
        Long newPid = resources.getPid();
        if (resources.getPid() != null && resources.getId().equals(resources.getPid())) {
            throw new BadRequestException("上级不能为自己");
        }
        Dept dept = deptRepository.findById(resources.getId()).orElseGet(Dept::new);
        ValidationUtil.isNull(dept.getId(), "Dept", "id", resources.getId());
        resources.setId(dept.getId());
        deptRepository.save(resources);
        // 更新父节点中子节点数目
        updateSubCnt(oldPid);
        updateSubCnt(newPid);
        // 清理缓存
        delCaches(resources.getId());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<DeptDto> deptDtos) {
        for (DeptDto deptDto : deptDtos) {
            // 清理缓存
            delCaches(deptDto.getId());
            deptRepository.deleteById(deptDto.getId());
            updateSubCnt(deptDto.getPid());
        }
    }

    @Override
    public void download(List<DeptDto> deptDtos, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (DeptDto deptDTO : deptDtos) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("部门名称", deptDTO.getName());
            map.put("部门状态", Boolean.TRUE.equals(deptDTO.getEnabled()) ? "启用" : "停用");
            map.put("创建日期", deptDTO.getCreateTime());
            list.add(map);
        }
        FileUtil.downloadExcel(list, response);
    }

    @Override
//    此类List入参的不建议缓存
    @Transactional(rollbackFor = Exception.class)
    public Set<DeptDto> getDeleteDepts(List<Dept> menuList, Set<DeptDto> deptDtos) {
        for (Dept dept : menuList) {
            deptDtos.add(deptMapper.toDto(dept, new CycleAvoidingMappingContext()));
            List<Dept> depts = deptRepository.findByPid(dept.getId());
            if (depts != null && !depts.isEmpty()) {
                getDeleteDepts(depts, deptDtos);
            }
        }
        return deptDtos;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Long> getDeptChildren(List<Dept> deptList) {
        var deptIds = deptList.stream().filter(Dept::getEnabled).map(Dept::getId).toList();
        if (deptIds.isEmpty())
            return Collections.emptyList();
        List<Long> list = new ArrayList<>(deptIds);
        var optionalDepts = deptRepository.findByPidIn(deptIds);
        optionalDepts.ifPresent(depts -> list.addAll(getDeptChildren(depts)));
        return list;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<DeptDto> getSuperior(DeptDto deptDto, List<Dept> depts) {
        if (deptDto.getPid() == null) {
            depts.addAll(deptRepository.findByPidIsNull());
            return deptMapper.toDto(depts, new CycleAvoidingMappingContext());
        }
        depts.addAll(deptRepository.findByPid(deptDto.getPid()));
        return getSuperior(findById(deptDto.getPid()), depts);
    }

    @Override
    public Object buildTree(List<DeptDto> deptDtos) {
        Set<DeptDto> trees = new LinkedHashSet<>();
        Set<DeptDto> depts = new LinkedHashSet<>();
        List<String> deptNames = deptDtos.stream().map(DeptDto::getName).toList();
        boolean isChild;
        for (DeptDto deptDTO : deptDtos) {
            isChild = false;
            if (deptDTO.getPid() == null) {
                trees.add(deptDTO);
            }
            for (DeptDto it : deptDtos) {
                if (it.getPid() != null && deptDTO.getId().equals(it.getPid())) {
                    isChild = true;
                    if (deptDTO.getChildren() == null) {
                        deptDTO.setChildren(new ArrayList<>());
                    }
                    deptDTO.getChildren().add(it);
                }
            }
            if (isChild) {
                depts.add(deptDTO);
            } else if (deptDTO.getPid() != null && !deptNames.contains(findById(deptDTO.getPid()).getName())) {
                depts.add(deptDTO);
            }
        }

        if (CollectionUtil.isEmpty(trees)) {
            trees = depts;
        }
        Map<String, Object> map = new HashMap<>(2);
        map.put("totalElements", deptDtos.size());
        map.put("content", CollectionUtil.isEmpty(trees) ? deptDtos : trees);
        return map;
    }

    @Override
    public void verification(Set<DeptDto> deptDtos) {
//        dto 2 entity
        var deptList = deptMapper.toEntity(new ArrayList<>(deptDtos), new CycleAvoidingMappingContext());
        if (Boolean.TRUE.equals(userService.hasDepts(deptList)))
            throw new BadRequestException("所选部门存在用户关联，请解除后再试！");
        if (Boolean.TRUE.equals(roleService.hasDepts(deptList)))
            throw new BadRequestException("所选部门存在角色关联，请解除后再试！");
    }

    private void updateSubCnt(Long deptId) {
        if (deptId != null) {
            int count = deptRepository.countByPid(deptId);
            deptRepository.updateSubCntById(count, deptId);
        }
    }

    private List<DeptDto> deduplication(List<DeptDto> list) {
        List<DeptDto> deptDtos = new ArrayList<>();
        for (DeptDto deptDto : list) {
            boolean flag = true;
            for (DeptDto dto : list) {
                if (dto.getId().equals(deptDto.getPid())) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                deptDtos.add(deptDto);
            }
        }
        return deptDtos;
    }

    /**
     * 清理缓存
     *
     * @param id /
     */
    public void delCaches(Long id) {
        // 发布部门更新事件
        notifyObserver(id);
    }
}
