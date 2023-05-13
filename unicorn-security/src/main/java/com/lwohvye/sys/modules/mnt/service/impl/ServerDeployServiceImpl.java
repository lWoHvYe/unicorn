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

import com.lwohvye.sys.modules.mnt.domain.ServerDeploy;
import com.lwohvye.sys.modules.mnt.service.dto.ServerDeployDto;
import com.lwohvye.sys.modules.mnt.service.dto.ServerDeployQueryCriteria;
import com.lwohvye.sys.modules.mnt.repository.ServerDeployRepository;
import com.lwohvye.sys.modules.mnt.service.IServerDeployService;
import com.lwohvye.sys.modules.mnt.util.ExecuteShellUtil;
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
public class ServerDeployServiceImpl implements IServerDeployService {

    private final ServerDeployRepository serverDeployRepository;

    private final ConversionService conversionService;

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(ServerDeployQueryCriteria criteria, Pageable pageable) {
        Page<ServerDeploy> page = serverDeployRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable);
        return PageUtils.toPage(page.map(serverDeploy -> conversionService.convert(serverDeploy, ServerDeployDto.class)));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<ServerDeployDto> queryAll(ServerDeployQueryCriteria criteria) {
        return serverDeployRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder))
                .stream().map(serverDeploy -> conversionService.convert(serverDeploy, ServerDeployDto.class)).toList();
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public ServerDeployDto findById(Long id) {
        ServerDeploy server = serverDeployRepository.findById(id).orElseGet(ServerDeploy::new);
        ValidationUtils.isNull(server.getId(), "ServerDeploy", "id", id);
        return conversionService.convert(server, ServerDeployDto.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public ServerDeployDto findByIp(String ip) {
        ServerDeploy deploy = serverDeployRepository.findByIp(ip);
        return conversionService.convert(deploy, ServerDeployDto.class);
    }

    @Override
    public Boolean testConnect(ServerDeploy resources) {
        ExecuteShellUtil executeShellUtil = null;
        try {
            executeShellUtil = new ExecuteShellUtil(resources.getIp(), resources.getAccount(), resources.getPassword(), resources.getPort());
            return executeShellUtil.execute("ls") == 0;
        } catch (Exception e) {
            return false;
        } finally {
            if (executeShellUtil != null) {
                executeShellUtil.close();
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void create(ServerDeploy resources) {
        serverDeployRepository.save(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(ServerDeploy resources) {
        ServerDeploy serverDeploy = serverDeployRepository.findById(resources.getId()).orElseGet(ServerDeploy::new);
        ValidationUtils.isNull(serverDeploy.getId(), "ServerDeploy", "id", resources.getId());
        serverDeploy.copy(resources);
        serverDeployRepository.save(serverDeploy);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<Long> ids) {
        for (Long id : ids) {
            serverDeployRepository.deleteById(id);
        }
    }

    @Override
    public void download(List<ServerDeployDto> queryAll, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (ServerDeployDto deployDto : queryAll) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("服务器名称", deployDto.getName());
            map.put("服务器IP", deployDto.getIp());
            map.put("端口", deployDto.getPort());
            map.put("账号", deployDto.getAccount());
            map.put("创建日期", deployDto.getCreateTime());
            list.add(map);
        }
        FileUtils.downloadExcel(list, response);
    }
}
