package com.lwohvye.config.redis;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettucePoolingClientConfiguration;

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
                .commandTimeout(this.properties.getTimeout())
                .poolConfig(poolConfig).build();
        return new LettuceConnectionFactory(configuration, clientConfiguration);
    }

    private GenericObjectPoolConfig<Object> poolConfig() {
        var poolConfig = new GenericObjectPoolConfig<>();
        RedisProperties.Pool props = this.properties.getLettuce().getPool();
        poolConfig.setMaxTotal(props.getMaxActive());
        poolConfig.setMaxIdle(props.getMaxIdle());
        poolConfig.setMinIdle(props.getMinIdle());
        return poolConfig;
    }
}
