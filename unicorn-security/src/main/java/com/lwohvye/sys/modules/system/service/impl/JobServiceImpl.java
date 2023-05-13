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

import com.lwohvye.api.modules.system.domain.Job;
import com.lwohvye.api.modules.system.service.dto.JobDto;
import com.lwohvye.api.modules.system.service.dto.JobQueryCriteria;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.*;
import com.lwohvye.sys.modules.system.repository.JobRepository;
import com.lwohvye.sys.modules.system.service.IJobService;
import com.lwohvye.sys.modules.system.service.IUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.EntityExistsException;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.*;

/**
 * @author Zheng Jie
 * @date 2019-03-29
 */
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "job")
public class JobServiceImpl implements IJobService {

    private final JobRepository jobRepository;

    private final ConversionService conversionService;
    private final IUserService userService;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(JobQueryCriteria criteria, Pageable pageable) {
        Page<Job> page = jobRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(job -> conversionService.convert(job, JobDto.class)));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<JobDto> queryAll(JobQueryCriteria criteria) {
        return jobRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder))
                .stream().map(job -> conversionService.convert(job, JobDto.class)).toList();
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    @Cacheable(key = " #root.target.getSysName() + 'id:' + #p0")
    public JobDto findById(Long id) {
        Job job = jobRepository.findById(id).orElseGet(Job::new);
        ValidationUtils.isNull(job.getId(), "Job", "id", id);
        return conversionService.convert(job, JobDto.class);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(Job resources) {
        Job job = jobRepository.findByName(resources.getName());
        if (job != null) {
            throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(Job.class, "name", resources.getName(), "existed"));
        }
        jobRepository.save(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
//    @CacheEvict(key = "'id:' + #p0.id")
    @CacheEvict(allEntries = true)
    public void update(Job resources) {
        Job job = jobRepository.findById(resources.getId()).orElseGet(Job::new);
        Job old = jobRepository.findByName(resources.getName());
        if (old != null && !old.getId().equals(resources.getId())) {
            throw new EntityExistsException(ExceptionMsgUtils.generateExcMsg(Job.class, "name", resources.getName(), "existed"));
        }
        ValidationUtils.isNull(job.getId(), "Job", "id", resources.getId());
        resources.setId(job.getId());
        jobRepository.save(resources);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<Long> ids) {
        jobRepository.deleteAllByIdIn(ids);
    }

    @Override
    public void download(List<JobDto> jobDtos, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (JobDto jobDTO : jobDtos) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("岗位名称", jobDTO.getName());
            map.put("岗位状态", jobDTO.getEnabled() ? "启用" : "停用");
            map.put("创建日期", jobDTO.getCreateTime());
            list.add(map);
        }
        FileUtils.downloadExcel(list, response);
    }

    @Override
    public void verification(Set<Long> ids) {
        if (userService.countByJobs(ids) > 0) {
            throw new BadRequestException("所选的岗位中存在用户关联，请解除关联再试！");
        }
    }
}
