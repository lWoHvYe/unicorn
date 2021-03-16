package com.lwohvye.config.redis;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.StringRedisTemplate;

/**
 * @author Hongyan Wang
 * @description Redis配置类
 * @date 2021/3/17 0:11
 */
@Configuration
@EnableConfigurationProperties(
        {
                RedisAutoConfiguration.MainRedisProperties.class,
                RedisAutoConfiguration.SlaveRedisProperties.class,
                RedisAutoConfiguration.Main2RedisProperties.class
        }
)
public class RedisAutoConfiguration {
    /**
     * @Primary 注解告诉Spring遇到多个bean的时候，它为默认
     * prefix  告诉spring读取前缀为spring.main-redis的配置信息
     */
    @Primary
    @ConfigurationProperties(prefix = "spring.main-redis")
    public static class MainRedisProperties extends RedisProperties {
    }

    @ConfigurationProperties(prefix = "spring.slave-redis")
    public static class SlaveRedisProperties extends RedisProperties {
    }

    @ConfigurationProperties(prefix = "spring.main2-redis")
    public static class Main2RedisProperties extends RedisProperties {
    }

    /**
     * 通过配置获取RedisConnectionFactory
     *
     * @param mainRedisProperties mainRedis的配置信息
     * @return
     */
    @Primary
    @Bean("mainRedisConnectionFactory")
    public static RedisConnectionFactory getMainRedisConnectionFactory(MainRedisProperties mainRedisProperties) {
        var factory = new RedisConnectionConfiguration(mainRedisProperties);
        return factory.redisConnectionFactory();
    }

    /**
     * 获取mainRedis的StringRedisTemplate实例
     *
     * @param redisConnectionFactory 使用@Qulifier指定需要的factory
     * @return template
     */
    @Primary
    @Bean("mainStringRedisTemplate")
    public static StringRedisTemplate getMainStringRedisTemplate(@Qualifier(value = "mainRedisConnectionFactory") RedisConnectionFactory redisConnectionFactory) {
        var template = new StringRedisTemplate();
        template.setConnectionFactory(redisConnectionFactory);
        return template;
    }

    @Bean("slaveRedisConnectionFactory")
    public static RedisConnectionFactory getSlaveRedisConnectionFactory(SlaveRedisProperties slaveRedisProperties) {
        var factory = new RedisConnectionConfiguration(slaveRedisProperties);
        return factory.redisConnectionFactory();
    }

    @Bean("slaveStringRedisTemplate")
    public static StringRedisTemplate getSlaveStringRedisTemplate(@Qualifier(value = "slaveRedisConnectionFactory") RedisConnectionFactory redisConnectionFactory) {
        var template = new StringRedisTemplate();
        template.setConnectionFactory(redisConnectionFactory);
        return template;
    }

    @Bean("main2RedisConnectionFactory")
    public static RedisConnectionFactory getMain2RedisConnectionFactory(Main2RedisProperties main2RedisProperties) {
        var factory = new RedisConnectionConfiguration(main2RedisProperties);
        return factory.redisConnectionFactory();
    }

    @Bean("main2StringRedisTemplate")
    public static StringRedisTemplate getMain2StringRedisTemplate(@Qualifier(value = "main2RedisConnectionFactory") RedisConnectionFactory redisConnectionFactory) {
        var template = new StringRedisTemplate();
        template.setConnectionFactory(redisConnectionFactory);
        return template;
    }
}
