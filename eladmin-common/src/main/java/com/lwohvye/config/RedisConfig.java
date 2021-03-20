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
package com.lwohvye.config;

import com.alibaba.fastjson.JSON;
import com.lwohvye.utils.serializer.FastJsonRedisSerializer;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.Cache;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.serializer.RedisSerializationContext;

import java.lang.reflect.Method;
import java.time.Duration;
import java.util.HashMap;

/**
 * @author Zheng Jie
 * @date 2018-11-24
 */
@Slf4j
@Configuration
@EnableCaching
@ConditionalOnClass(RedisOperations.class)
@EnableConfigurationProperties(RedisProperties.class)
public class RedisConfig extends CachingConfigurerSupport {

    /**
     * 设置 redis 数据默认过期时间，默认2小时
     * 设置@cacheable 序列化方式
     */
    @Bean
    public RedisCacheConfiguration redisCacheConfiguration() {
        FastJsonRedisSerializer<Object> fastJsonRedisSerializer = new FastJsonRedisSerializer<>(Object.class);
        RedisCacheConfiguration configuration = RedisCacheConfiguration.defaultCacheConfig();
        configuration = configuration.serializeValuesWith(RedisSerializationContext.
                SerializationPair.fromSerializer(fastJsonRedisSerializer)).entryTtl(Duration.ofHours(6));
        return configuration;
    }

    // @SuppressWarnings("all")
//    @Bean(name = "redisTemplate")
//    @ConditionalOnMissingBean(name = "redisTemplate")
//    public RedisTemplate<Object, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory) {
//        RedisTemplate<Object, Object> template = new RedisTemplate<>();
//        //序列化
//        FastJsonRedisSerializer<Object> fastJsonRedisSerializer = new FastJsonRedisSerializer<>(Object.class);
//        // value值的序列化采用fastJsonRedisSerializer
//        template.setValueSerializer(fastJsonRedisSerializer);
//        template.setHashValueSerializer(fastJsonRedisSerializer);
//        // 全局开启AutoType，这里方便开发，使用全局的方式
//        ParserConfig.getGlobalInstance().setAutoTypeSupport(true);
//        // 建议使用这种方式，小范围指定白名单
//        // ParserConfig.getGlobalInstance().addAccept("com.lwohvye.domain");
//        // key的序列化采用StringRedisSerializer
//        template.setKeySerializer(new StringRedisSerializer());
//        template.setHashKeySerializer(new StringRedisSerializer());
//        template.setConnectionFactory(redisConnectionFactory);
//        return template;
//    }

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
            var container = new HashMap<String, Object>(3);
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
            String jsonString = JSON.toJSONString(container);
            // 做SHA256 Hash计算，得到一个SHA256摘要作为Key
            var sha256Hex = DigestUtils.sha256Hex(jsonString);
            var classSimpleName = targetClassClass.getSimpleName();
//            使用类名 + 方法名 + 摘要 做key，便于识别
            return classSimpleName + "::" + methodName + "::" + sha256Hex;
        };
    }

    @Bean
    @Override
    public CacheErrorHandler errorHandler() {
        // 异常处理，当Redis发生异常时，打印日志，但是程序正常走
        log.info("初始化 -> [{}]", "Redis CacheErrorHandler");
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

///**
// * Value 序列化
// *
// * @param <T>
// * @author /
// */
//class FastJsonRedisSerializer<T> implements RedisSerializer<T> {
//
//    private final Class<T> clazz;
//
//    FastJsonRedisSerializer(Class<T> clazz) {
//        super();
//        this.clazz = clazz;
//    }
//
//    @Override
//    public byte[] serialize(T t) {
//        if (t == null) {
//            return new byte[0];
//        }
//        return JSON.toJSONString(t, SerializerFeature.WriteClassName).getBytes(StandardCharsets.UTF_8);
//    }
//
//    @Override
//    public T deserialize(byte[] bytes) {
//        if (bytes == null || bytes.length <= 0) {
//            return null;
//        }
//        String str = new String(bytes, StandardCharsets.UTF_8);
//        return JSON.parseObject(str, clazz);
//    }
//
//}
//
///**
// * 重写序列化器
// *
// * @author /
// */
//class StringRedisSerializer implements RedisSerializer<Object> {
//
//    private final Charset charset;
//
//    StringRedisSerializer() {
//        this(StandardCharsets.UTF_8);
//    }
//
//    private StringRedisSerializer(Charset charset) {
//        Assert.notNull(charset, "Charset must not be null!");
//        this.charset = charset;
//    }
//
//    @Override
//    public String deserialize(byte[] bytes) {
//        return (bytes == null ? null : new String(bytes, charset));
//    }
//
//    @Override
//    public byte[] serialize(Object object) {
//        String string = JSON.toJSONString(object);
//        if (StringUtils.isBlank(string)) {
//            return null;
//        }
//        string = string.replace("\"", "");
//        return string.getBytes(charset);
//    }
//}
