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

import com.lwohvye.api.modules.system.domain.Dict;
import com.lwohvye.api.modules.system.service.dto.DictDto;
import com.lwohvye.api.modules.system.service.dto.DictQueryCriteria;
import com.lwohvye.core.utils.PageUtils;
import com.lwohvye.core.utils.QueryHelp;
import com.lwohvye.core.utils.ValidationUtils;
import com.lwohvye.sys.modules.system.repository.DictRepository;
import com.lwohvye.sys.modules.system.service.IDictService;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2019-04-10
 */
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "dict")
public class DictServiceImpl implements IDictService {

    private final DictRepository dictRepository;

    private final ConversionService conversionService;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(DictQueryCriteria criteria, Pageable pageable) {
        Page<Dict> page = dictRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(dict -> conversionService.convert(dict, DictDto.class)));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<DictDto> queryAll(DictQueryCriteria criteria) {
        return dictRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder))
                .stream().map(dict -> conversionService.convert(dict, DictDto.class)).toList();
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(Dict resources) {
        dictRepository.save(resources);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(Dict resources) {
        Dict dict = dictRepository.findById(resources.getId()).orElseGet(Dict::new);
        ValidationUtils.isNull(dict.getId(), "Dict", "id", resources.getId());
//        名称及描述允许置空
        dict.setName(resources.getName());
        dict.setDescription(resources.getDescription());
        dictRepository.save(dict);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<Long> ids) {
        dictRepository.deleteByIdIn(ids);
    }

}
