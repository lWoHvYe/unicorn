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
package com.lwohvye.sys.modules.system.service.impl;

import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.api.modules.system.service.dto.ResourceDto;
import com.lwohvye.api.modules.system.service.dto.ResourceQueryCriteria;
import com.lwohvye.sys.common.constant.SysCacheKey;
import com.lwohvye.core.utils.*;
import com.lwohvye.core.utils.redis.RedisUtils;
import com.lwohvye.sys.modules.system.event.ResEvent;
import com.lwohvye.sys.modules.system.event.RoleEvent;
import com.lwohvye.sys.modules.system.repository.ResourceRepository;
import com.lwohvye.sys.modules.system.service.IResourceService;
import lombok.RequiredArgsConstructor;
import org.jetbrains.annotations.NotNull;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;
import org.springframework.context.event.EventListener;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 服务实现
 *
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "resource")
public class ResourceServiceImpl implements IResourceService, ApplicationEventPublisherAware {

    private final ResourceRepository resourceRepository;

    private final ConversionService conversionService;
    private final RedisUtils redisUtils;

    private ApplicationEventPublisher eventPublisher;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(ResourceQueryCriteria criteria, Pageable pageable) {
        Page<Resource> page = resourceRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(resource -> conversionService.convert(resource, ResourceDto.class)));
    }

    @Override
    public List<ResourceDto> queryAll(ResourceQueryCriteria criteria) {
        return resourceRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder))
                .stream().map(resource -> conversionService.convert(resource, ResourceDto.class)).toList();
    }

    @Override
    @Cacheable(key = "'allResources' ")
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<ResourceDto> queryAllRes() {
        // 序列化是一个数组，无法反序列化。序列化结果为：[{“@class”:”xxx”,”name”:”xx”},{“@class”:”xxx”,”name”:”xx”}]
        // return resourceRepository.findAll().stream().map(resource -> conversionService.convert(resource, ResourceDto.class)).toList();
        // 这种可以。[“java.util.ArrayList”,[{“@class”:”xxx”,”name”:”xx”},{“@class”:”xxx”,”name”:”xx”}]]
        return new ArrayList<>(resourceRepository.findAll().stream().map(resource -> conversionService.convert(resource, ResourceDto.class)).toList());
        // 之前的这种可以，是因为内部有new ArrayList
        // return resourceMapper.toDto(list, new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public ResourceDto findById(Long resourceId) {
        Resource resource = resourceRepository.findById(resourceId).orElseGet(Resource::new);
        ValidationUtils.isNull(resource.getResourceId(), "Resource", "resourceId", resourceId);
        return conversionService.convert(resource, ResourceDto.class);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public ResourceDto create(Resource resources) {
        var save = resourceRepository.save(resources);
        // publishEvent
        publishResEvent(save);
        return conversionService.convert(save, ResourceDto.class);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(Resource resources) {
        Resource resource = resourceRepository.findById(resources.getResourceId()).orElseGet(Resource::new);
        ValidationUtils.isNull(resource.getResourceId(), "Resource", "id", resources.getResourceId());
        resource.copy(resources);
        resourceRepository.save(resource);
        publishResEvent(resource);
    }

    @Override
    @CacheEvict(allEntries = true)
    public void deleteAll(Long[] ids) {
        for (Long resourceId : ids) {
            resourceRepository.deleteById(resourceId);
        }
        publishResEvent(null);
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
        FileUtils.downloadExcel(list, response);
    }

    @Override
    public void setApplicationEventPublisher(@NotNull ApplicationEventPublisher applicationEventPublisher) {
        eventPublisher = applicationEventPublisher;
    }

    public void publishResEvent(Resource res) {
        eventPublisher.publishEvent(new ResEvent(this, res));
    }

    @EventListener
    public void objUpdate(RoleEvent roleEvent) {
        redisUtils.delInRC(SysCacheKey.RESOURCE_ALL, null);
    }
}
