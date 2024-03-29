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
package com.lwohvye.sys.modules.system.service;

import com.lwohvye.api.modules.system.domain.Dict;
import com.lwohvye.api.modules.system.service.dto.DictDto;
import com.lwohvye.api.modules.system.service.dto.DictQueryCriteria;
import com.lwohvye.core.base.BaseService;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Zheng Jie
 * @date 2019-04-10
 */
public interface IDictService extends BaseService {

    /**
     * 分页查询
     *
     * @param criteria 条件
     * @param pageable 分页参数
     */
    Map<String, Object> queryAll(DictQueryCriteria criteria, Pageable pageable);

    /**
     * 查询全部数据
     *
     * @param dict
     */
    List<DictDto> queryAll(DictQueryCriteria dict);

    /**
     * 创建
     *
     * @param resources
     */
    void create(Dict resources);

    /**
     * 编辑
     *
     * @param resources /
     */
    void update(Dict resources);

    /**
     * 删除
     *
     * @param ids /
     */
    void delete(Set<Long> ids);
}
