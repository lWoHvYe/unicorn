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

package com.unicorn.ai.support.config;

import com.unicorn.ai.support.config.SupportProperties;
import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.vectorstore.SimpleVectorStore;
import org.springframework.ai.vectorstore.VectorStore;
import org.springframework.ai.vectorstore.pgvector.PgVectorStore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;

@Configuration
public class VectorStoreConfig {

    @Bean
    @ConditionalOnProperty(name = "ai.support.vector-store", havingValue = "simple", matchIfMissing = true)
    VectorStore simpleVectorStore(EmbeddingModel embeddingModel) {
        return SimpleVectorStore.builder(embeddingModel).build();
    }

    @Bean
    @ConditionalOnProperty(name = "ai.support.vector-store", havingValue = "pgvector")
    VectorStore pgVectorStore(JdbcTemplate jdbcTemplate, EmbeddingModel embeddingModel,
                              SupportProperties properties) {
        var pg = properties.pgvector();
        return PgVectorStore.builder(jdbcTemplate, embeddingModel)
                .schemaName(pg.schemaName())
                .vectorTableName(pg.tableName())
                .dimensions(pg.dimensions())
                .distanceType(PgVectorStore.PgDistanceType.COSINE_DISTANCE)
                .indexType(PgVectorStore.PgIndexType.HNSW)
                .initializeSchema(true)
                .build();
    }
}
