/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.lwohvye.search.modules.elasticsearch.service.impl;

import cn.hutool.core.util.RandomUtil;
import com.lwohvye.search.modules.elasticsearch.domain.EsUser;
import com.lwohvye.search.modules.elasticsearch.repository.EsUserRepository;
import com.lwohvye.search.modules.elasticsearch.service.IEsUserService;
import com.lwohvye.search.modules.mongodb.repository.MongoDBUserRepository;
import com.lwohvye.core.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.fetch.subphase.highlight.HighlightBuilder;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class EsUserServiceImpl implements IEsUserService {

    @Autowired
    private EsUserRepository esUserRepository;

    @Autowired
    private MongoDBUserRepository mongoDBUserRepository;
    // 使用ElasticsearchRestTemplate的search()方法进行复杂查询
    @Autowired
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Override
    public Object queryAll() {
        // 复杂查询
        var searchQuery = new NativeSearchQueryBuilder()
                .withQuery(QueryBuilders.queryStringQuery("admin").defaultField("userName")) // 查询条件
                .withPageable(PageRequest.of(0, 5)) // 分页
                .withSorts(SortBuilders.fieldSort("userName").order(SortOrder.DESC)) // 排序。只能对keywords的样子，id不行
                .withHighlightFields(new HighlightBuilder.Field("adm")) // 高亮字段显示
                .build();
        var searchHits = elasticsearchRestTemplate.search(searchQuery, EsUser.class);
        return searchHits.getTotalHits() <= 0L ?
                esUserRepository.findAll()
                : PageUtils.toPage(searchHits.stream().map(SearchHit::getContent).toList(), searchHits.getTotalHits());
    }

    @Override
    public void updateUsers() {
//        esUserRepository.deleteAll();
        mongoDBUserRepository.findAll().parallelStream().forEach(mongoDBUser -> {
            var userName = mongoDBUser.getUserName();
            // 查询
            var esUser = esUserRepository.readByUserName(userName).orElseGet(EsUser::new);
            // 赋值
            esUser.setId(mongoDBUser.getId()).setUserName(mongoDBUser.getUserName()).setPassWord(mongoDBUser.getPassWord())
                    .setRoleName(mongoDBUser.getRoleName()).setSex(RandomUtil.randomBoolean() ? 0 : 1);

            esUserRepository.save(esUser);
        });

    }
}
