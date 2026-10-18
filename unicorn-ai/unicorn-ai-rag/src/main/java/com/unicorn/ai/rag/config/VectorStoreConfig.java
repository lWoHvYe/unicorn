/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.unicorn.ai.rag.config;

import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.vectorstore.SimpleVectorStore;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.ai.vectorstore.redis.RedisVectorStore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;
import redis.clients.jedis.RedisClient;

@Configuration
@EnableConfigurationProperties(RagProperties.class)
public class VectorStoreConfig {

    /**
     * 默认零依赖：向量存在进程内，重启即丢，适合 Demo 与单元/集成测试。
     */
    @Bean
    @ConditionalOnProperty(name = "demo.ai.rag.vector-store", havingValue = "simple", matchIfMissing = true)
    VectorStore simpleVectorStore(EmbeddingModel embeddingModel) {
        return SimpleVectorStore.builder(embeddingModel).build();
    }

    /**
     * Redis Stack（Redis Query Engine + RediSearch）方案；需要 redis/redis-stack 镜像。
     */
    @Bean
    @ConditionalOnProperty(name = "demo.ai.rag.vector-store", havingValue = "redis")
    VectorStore redisVectorStore(EmbeddingModel embeddingModel, RagProperties properties) {
        var redis = properties.redis();
        var client = StringUtils.hasText(redis.password())
                ? RedisClient.create(redis.host(), redis.port(), null, redis.password())
                : RedisClient.create(redis.host(), redis.port());
        return RedisVectorStore.builder(client, embeddingModel)
                .indexName(redis.indexName())
                .prefix(redis.prefix())
                .metadataFields(
                        RedisVectorStore.MetadataField.tag("source"),
                        RedisVectorStore.MetadataField.tag("tenant"),
                        RedisVectorStore.MetadataField.tag("docType"))
                .initializeSchema(true)
                .build();
    }
}
