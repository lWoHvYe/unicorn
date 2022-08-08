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
package com.lwohvye.sys.modules.system.service;

import com.lwohvye.api.modules.system.domain.Resource;
import com.lwohvye.core.base.BaseService;
import com.lwohvye.api.modules.system.service.dto.ResourceDto;
import com.lwohvye.api.modules.system.service.dto.ResourceQueryCriteria;
import org.springframework.data.domain.Pageable;

import java.util.Map;
import java.util.List;
import java.io.IOException;
import javax.servlet.http.HttpServletResponse;

/**
 * 服务接口
 *
 * @author Super idol lv
 * @website https://el-admin.vip
 * @date 2021-11-27
 **/
public interface IResourceService extends BaseService {

    /**
     * 查询数据分页
     *
     * @param criteria 条件
     * @param pageable 分页参数
     * @return Map<String, Object>
     */
    Map<String, Object> queryAll(ResourceQueryCriteria criteria, Pageable pageable);

    /**
     * 查询所有数据不分页
     *
     * @param criteria 条件参数
     * @return List<ResourceDto>
     */
    List<ResourceDto> queryAll(ResourceQueryCriteria criteria);

    List<ResourceDto> queryAllRes();

    /**
     * 根据ID查询
     *
     * @param resourceId ID
     * @return ResourceDto
     */
    ResourceDto findById(Long resourceId);

    /**
     * 创建
     *
     * @param resources /
     * @return ResourceDto
     */
    ResourceDto create(Resource resources);

    /**
     * 编辑
     *
     * @param resources /
     */
    void update(Resource resources);

    /**
     * 多选删除
     *
     * @param ids /
     */
    void deleteAll(Long[] ids);

    /**
     * 导出数据
     *
     * @param all      待导出的数据
     * @param response /
     * @throws IOException /
     */
    void download(List<ResourceDto> all, HttpServletResponse response) throws IOException;

}
