/*
 *  Copyright 2020-2022 lWoHvYe
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
package com.lwohvye.config.redis;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettucePoolingClientConfiguration;

import java.time.Duration;

public class RedisConnectionConfiguration {

    private final RedisProperties properties;

    RedisConnectionConfiguration(RedisProperties properties) {
        this.properties = properties;
    }

    LettuceConnectionFactory redisConnectionFactory() {
        return createLettuceConnectionFactory();
    }


    private LettuceConnectionFactory createLettuceConnectionFactory() {
        var configuration = new RedisStandaloneConfiguration();
        configuration.setHostName(this.properties.getHost());
        configuration.setPort(this.properties.getPort());
        if (this.properties.getPassword() != null) {
            configuration.setPassword(this.properties.getPassword());
        }
        if (this.properties.getDatabase() != 0) {
            configuration.setDatabase(this.properties.getDatabase());
        }

        var poolConfig = poolConfig();
        var clientConfiguration = LettucePoolingClientConfiguration.builder()
                .commandTimeout(Duration.ofMillis(this.properties.getTimeout()))
                .poolConfig(poolConfig).build();
        return new LettuceConnectionFactory(configuration, clientConfiguration);
    }

    private GenericObjectPoolConfig<Object> poolConfig() {
        var poolConfig = new GenericObjectPoolConfig<>();
        RedisProperties.Pool props = this.properties.getPool();
        poolConfig.setMaxTotal(props.getMaxActive());
        poolConfig.setMaxIdle(props.getMaxIdle());
        poolConfig.setMinIdle(props.getMinIdle());
        return poolConfig;
    }
}
