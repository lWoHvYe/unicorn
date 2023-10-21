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

import com.lwohvye.api.modules.system.domain.DictDetail;
import com.lwohvye.api.modules.system.service.dto.DictDetailDto;
import com.lwohvye.api.modules.system.service.dto.DictDetailQueryCriteria;
import com.lwohvye.sys.modules.system.repository.DictDetailRepository;
import com.lwohvye.sys.modules.system.service.IDictDetailService;
import com.lwohvye.core.utils.PageUtils;
import com.lwohvye.core.utils.QueryHelp;
import com.lwohvye.core.utils.ValidationUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Zheng Jie
 * @date 2019-04-10
 */
@Service
@RequiredArgsConstructor
// 这里使用dict做域名，而非dictName，需注意
@CacheConfig(cacheNames = "dict")
public class DictDetailServiceImpl implements IDictDetailService {

    private final DictDetailRepository dictDetailRepository;

    private final ConversionService conversionService;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(DictDetailQueryCriteria criteria, Pageable pageable) {
        Page<DictDetail> page = dictDetailRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(dictDetail -> conversionService.convert(dictDetail, DictDetailDto.class)));
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(DictDetail resources) {
        dictDetailRepository.save(resources);
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(DictDetail resources) {
        DictDetail dictDetail = dictDetailRepository.findById(resources.getId()).orElseGet(DictDetail::new);
        ValidationUtils.isNull(dictDetail.getId(), "DictDetail", "id", resources.getId());
        resources.setId(dictDetail.getId());
        dictDetailRepository.save(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    @Cacheable(key = "'name:' + #p0")
    public List<DictDetailDto> getDictByName(String name) {
        return new ArrayList<>(dictDetailRepository.findByDictName(name).stream().map(dictDetail -> conversionService.convert(dictDetail, DictDetailDto.class)).toList());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long id) {
        dictDetailRepository.deleteById(id);
    }

}
