/*
 *  Copyright 2019-2022 lWoHvYe
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
package com.lwohvye.modules.system.service.impl;

import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.modules.system.domain.Resource;
import com.lwohvye.modules.system.repository.ResourceRepository;
import com.lwohvye.modules.system.service.IResourceService;
import com.lwohvye.modules.system.service.dto.ResourceDto;
import com.lwohvye.modules.system.service.dto.ResourceQueryCriteria;
import com.lwohvye.modules.system.service.mapstruct.ResourceMapper;
import com.lwohvye.utils.FileUtil;
import com.lwohvye.utils.PageUtil;
import com.lwohvye.utils.QueryHelp;
import com.lwohvye.utils.ValidationUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Super idol lv
 * @website https://el-admin.vip
 * @description 服务实现
 * @date 2021-11-27
 **/
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "resource")
public class ResourceServiceImpl implements IResourceService {

    private final ResourceRepository resourceRepository;
    private final ResourceMapper resourceMapper;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> queryAll(ResourceQueryCriteria criteria, Pageable pageable) {
        Page<Resource> page = resourceRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtil.toPage(page.map(resource -> resourceMapper.toDto(resource, new CycleAvoidingMappingContext())));
    }

    @Override
    public List<ResourceDto> queryAll(ResourceQueryCriteria criteria) {
        var list = resourceRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder));
        return resourceMapper.toDto(list, new CycleAvoidingMappingContext());
    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public List<ResourceDto> queryAllRes() {
        return resourceMapper.toDto(resourceRepository.findAll(), new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional
    public ResourceDto findById(Long resourceId) {
        Resource resource = resourceRepository.findById(resourceId).orElseGet(Resource::new);
        ValidationUtil.isNull(resource.getResourceId(), "Resource", "resourceId", resourceId);
        return resourceMapper.toDto(resource, new CycleAvoidingMappingContext());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public ResourceDto create(Resource resources) {
        var save = resourceRepository.save(resources);
        return resourceMapper.toDto(save, new CycleAvoidingMappingContext());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(Resource resources) {
        Resource resource = resourceRepository.findById(resources.getResourceId()).orElseGet(Resource::new);
        ValidationUtil.isNull(resource.getResourceId(), "Resource", "id", resources.getResourceId());
        resource.copy(resources);
        resourceRepository.save(resource);
    }

    @Override
    @CacheEvict(allEntries = true)
    public void deleteAll(Long[] ids) {
        for (Long resourceId : ids) {
            resourceRepository.deleteById(resourceId);
        }
    }

    @Override
    public void download(List<ResourceDto> all, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (ResourceDto resource : all) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("资源名称", resource.getName());
            map.put("URI", resource.getPattern());
            map.put("状态 0-不可用 1-可用", resource.getStatus());
            map.put("所在类名", resource.getRestName());
            map.put("备注", resource.getRemark());
            list.add(map);
        }
        FileUtil.downloadExcel(list, response);
    }
}
