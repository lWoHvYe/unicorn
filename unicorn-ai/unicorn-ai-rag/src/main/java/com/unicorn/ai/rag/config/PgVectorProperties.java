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

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

/**
 * docker-compose-env-sim.yml 中的 pgvector 服务：
 * jdbc:postgresql://10.211.55.29:5434/vectordb，postgres/postgres。
 */
@ConfigurationProperties("demo.ai.rag.pgvector")
public record PgVectorProperties(
        @DefaultValue("jdbc:postgresql://10.211.55.29:5434/vectordb") String url,
        @DefaultValue("postgres") String username,
        @DefaultValue("postgres") String password,
        @DefaultValue("unicorn_ai_rag") String tableName,
        @DefaultValue("public") String schemaName,
        // text-embedding-3-small 默认 1536 维；换模型时覆盖
        @DefaultValue("1536") int dimensions) {
}
