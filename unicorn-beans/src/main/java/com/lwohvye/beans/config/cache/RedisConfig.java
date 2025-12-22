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
package com.lwohvye.beans.config.cache;


import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.lwohvye.beans.config.LocalPropertyConfig;
import com.lwohvye.core.custom.ConcurrentFreshCacheManager;
import com.lwohvye.core.utils.json.JsonUtils;
import com.lwohvye.core.utils.redis.RedisUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.redisson.api.RedissonClient;
import org.redisson.spring.cache.RedissonSpringCacheManager;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.CachingConfigurer;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Role;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.JacksonJsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import tools.jackson.databind.DefaultTyping;
import tools.jackson.databind.DeserializationFeature;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.jsontype.BasicPolymorphicTypeValidator;

import java.lang.reflect.Method;
import java.util.HashMap;

/**
 * @author Zheng Jie
 * @date 2018-11-24
 */
@Slf4j
@Configuration
@EnableCaching
@ConditionalOnClass(RedisOperations.class)
@Role(BeanDefinition.ROLE_INFRASTRUCTURE)
public class RedisConfig implements CachingConfigurer {

    // 这个是替换原来的RedisCacheManager的。通过该CacheManager，使用Cacheable 注解，缓存数据会被放在一个RMap 中，搞清楚这点后，可以比较精准的清除一些key
    @Bean
    @Primary
    CacheManager redissonCacheManager(RedissonClient redissonClient) {
        return new RedissonSpringCacheManager(redissonClient, "classpath:cache-config.yaml");
    }

    // not suitable for caffeine
    @Bean
    CacheManager localCacheManager() {
        return new ConcurrentFreshCacheManager();
    }

    /**
     * 与RedisUtil一起使用，完全整合Redisson后，RedisUtil应该就用不到了
     *
     * @param redisConnectionFactory /
     * @return org.springframework.data.redis.core.RedisTemplate
     * @date 2021/11/11 1:25 上午
     */
    @Bean(name = "redisTemplate")
    @ConditionalOnMissingBean(name = "redisTemplate")
    public RedisTemplate<Object, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory) {
        var template = new RedisTemplate<>();
        // 序列化
        var stringRedisSerializer = new StringRedisSerializer();
        // 使用Jackson2JsonRedisSerializer来序列化和反序列化redis的value值
        var jacksonJsonRedisSerializer = jacksonJsonRedisSerializer();
        // key的序列化采用StringRedisSerializer
        template.setKeySerializer(stringRedisSerializer);
        template.setHashKeySerializer(stringRedisSerializer);
        // value值的序列化采用jackson2JsonRedisSerializer
        template.setValueSerializer(jacksonJsonRedisSerializer);
        template.setHashValueSerializer(jacksonJsonRedisSerializer);

        template.setConnectionFactory(redisConnectionFactory);
        //执行afterPropertiesSet方法，完成属性的设置
        template.afterPropertiesSet();
        return template;
    }


    private JacksonJsonRedisSerializer<Object> jacksonJsonRedisSerializer() {
        var objectMapper = JsonMapper.builder()
                // 如果json中有新增的字段并且是实体类类中不存在的，不报错。即允许json串中有，而pojo中没有的属性
                .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
                // 指定要序列化的域，field,get和set,以及修饰符范围，ANY是都有包括private和public
                .changeDefaultVisibility(vc -> vc.withVisibility(PropertyAccessor.ALL, JsonAutoDetect.Visibility.ANY))
                // todo 需要看一下怎么配置下面这个，不然有的功能可能报错
                 .activateDefaultTyping(BasicPolymorphicTypeValidator.builder().build(), DefaultTyping.JAVA_LANG_OBJECT, JsonTypeInfo.As.PROPERTY)
                .build();
        return new JacksonJsonRedisSerializer<>(objectMapper, Object.class);
    }

    /**
     * 自定义缓存key生成策略，默认将使用该策略。针对查询，使用toString作为key
     * 列表查询：放入缓存（包含条件筛选）
     * 新增操作：清除列表查询缓存。暂不做加入缓存操作
     * 修改操作：清除列表查询缓存、清除该记录相关的其他缓存（比如findById等）。暂不做加入缓存操作
     * 删除操作：清除列表查询缓存、清除该记录相关的其他缓存（比如findById等）。暂不做加入缓存操作
     */
    @Bean
    @Override
    public KeyGenerator keyGenerator() {
        return (Object target, Method method, Object... params) -> {
            // HashMap指定初始容量时，会根据给定的值，查找并设置不小于且距离给定值最近的2的幂为真正的初始容量。
            // 因为若x为2的幂，则y%x = y&(x-1)，
            // 默认初始容量为16，负载因子为0.75f。这里指定为8，当put的内容超过6时，将触发扩容，在当下一般一两个参数的情况下，够用了
            var container = new HashMap<String, Object>(8);
            var targetClassClass = target.getClass();
            var methodName = method.getName();
            // 类地址。可根据需要决定是否放入摘要中
            container.put("class", targetClassClass.toGenericString());
            // 方法名称。可根据需要决定是否放入摘要中
            container.put("methodName", methodName);
            // 包名称。可根据需要决定是否放入摘要中
            container.put("package", targetClassClass.getPackage());
            // 参数列表
            for (int i = 0; i < params.length; i++) {
                container.put(String.valueOf(i), params[i]);
            }
            // 转为JSON字符串
            String jsonString = JsonUtils.toJSONString(container);
            // 做SHA256 Hash计算，得到一个SHA256摘要作为Key
            var sha256Hex = DigestUtils.sha256Hex(jsonString);
            var classSimpleName = targetClassClass.getSimpleName();
//            使用类名 + 方法名 + 摘要 做key，便于识别
            return LocalPropertyConfig.SYS_NAME + classSimpleName + "::" + methodName + "::" + sha256Hex;
        };
    }

    @Bean
    public RedisUtils redisUtils(RedisTemplate<Object, Object> redisTemplate, StringRedisTemplate stringRedisTemplate, RedissonClient redissonClient) {
        return new RedisUtils(redisTemplate, stringRedisTemplate, redissonClient);
    }

    @Bean
    @Role(BeanDefinition.ROLE_INFRASTRUCTURE)
    @Override
    public CacheErrorHandler errorHandler() {
        // 异常处理，当Redis发生异常时，打印日志，但是程序正常走
        log.info(" Init -> [{}]", "Redis CacheErrorHandler");
        return new CacheErrorHandler() {
            @Override
            public void handleCacheGetError(RuntimeException e, Cache cache, Object key) {
                log.error("Redis occur handleCacheGetError：key -> [{}]", key, e);
            }

            @Override
            public void handleCachePutError(RuntimeException e, Cache cache, Object key, Object value) {
                log.error("Redis occur handleCachePutError：key -> [{}]；value -> [{}]", key, value, e);
            }

            @Override
            public void handleCacheEvictError(RuntimeException e, Cache cache, Object key) {
                log.error("Redis occur handleCacheEvictError：key -> [{}]", key, e);
            }

            @Override
            public void handleCacheClearError(RuntimeException e, Cache cache) {
                log.error("Redis occur handleCacheClearError：", e);
            }
        };
    }

}
