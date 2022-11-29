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
package com.lwohvye.sys.modules.mnt.service.impl;

import cn.hutool.core.util.IdUtil;
import com.lwohvye.api.modules.mnt.domain.DeployHistory;
import com.lwohvye.api.modules.mnt.service.dto.DeployHistoryDto;
import com.lwohvye.api.modules.mnt.service.dto.DeployHistoryQueryCriteria;
import com.lwohvye.sys.modules.mnt.repository.DeployHistoryRepository;
import com.lwohvye.sys.modules.mnt.service.IDeployHistoryService;
import com.lwohvye.core.utils.FileUtils;
import com.lwohvye.core.utils.PageUtils;
import com.lwohvye.core.utils.QueryHelp;
import com.lwohvye.core.utils.ValidationUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

/**
 * @author zhanghouying
 * @date 2019-08-24
 */
@Service
@RequiredArgsConstructor
public class DeployHistoryServiceImpl implements IDeployHistoryService {

    private final DeployHistoryRepository deployHistoryRepository;

    private final ConversionService conversionService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> queryAll(DeployHistoryQueryCriteria criteria, Pageable pageable) {
        Page<DeployHistory> page = deployHistoryRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(deployHistory -> conversionService.convert(deployHistory, DeployHistoryDto.class)));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<DeployHistoryDto> queryAll(DeployHistoryQueryCriteria criteria) {
        return deployHistoryRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder))
                .stream().map(deployHistory -> conversionService.convert(deployHistory, DeployHistoryDto.class)).toList();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DeployHistoryDto findById(String id) {
        DeployHistory deployhistory = deployHistoryRepository.findById(id).orElseGet(DeployHistory::new);
        ValidationUtils.isNull(deployhistory.getId(), "DeployHistory", "id", id);
        return conversionService.convert(deployhistory, DeployHistoryDto.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void create(DeployHistory resources) {
        resources.setId(IdUtil.simpleUUID());
        deployHistoryRepository.save(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<String> ids) {
        for (String id : ids) {
            deployHistoryRepository.deleteById(id);
        }
    }

    @Override
    public void download(List<DeployHistoryDto> queryAll, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (DeployHistoryDto deployHistoryDto : queryAll) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("部署编号", deployHistoryDto.getDeployId());
            map.put("应用名称", deployHistoryDto.getAppName());
            map.put("部署IP", deployHistoryDto.getIp());
            map.put("部署时间", deployHistoryDto.getDeployDate());
            map.put("部署人员", deployHistoryDto.getDeployUser());
            list.add(map);
        }
        FileUtils.downloadExcel(list, response);
    }
}
