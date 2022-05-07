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
package com.lwohvye.config.cache;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.impl.LaissezFaireSubTypeValidator;
import com.lwohvye.config.LocalCoreConfig;
import com.lwohvye.utils.json.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.Config;
import org.redisson.spring.cache.RedissonSpringCacheManager;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import java.io.IOException;
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
@EnableConfigurationProperties(RedisProperties.class)
public class RedisConfig extends CachingConfigurerSupport {

    /**
     * 设置 redis 数据默认过期时间，默认2小时
     * 设置@Cacheable 序列化方式
     */
    // @Bean
    // public RedisCacheManager redisCacheManager(RedisConnectionFactory redisConnectionFactory) {
    //     var stringRedisSerializer = new StringRedisSerializer();
    //     Jackson2JsonRedisSerializer<Object> jackson2JsonRedisSerializer = jackson2JsonRedisSerializer();
    //     var configuration = RedisCacheConfiguration.defaultCacheConfig()
    //             // key序列化
    //             .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer))
    //             // value序列化
    //             .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(jackson2JsonRedisSerializer))
    //             .entryTtl(Duration.ofHours(2)).disableCachingNullValues();
    //     return RedisCacheManager.builder(redisConnectionFactory).cacheDefaults(configuration).build();
    // }

    // 默认读的json格式，yaml需要这样额外配置
    @Bean(destroyMethod = "shutdown")
    RedissonClient redisson(@Value("classpath:/redisson.yaml") Resource configFile) throws IOException {
        Config config = Config.fromYAML(configFile.getInputStream());
        return Redisson.create(config);
    }

    // 这个是替换原来的RedisCacheManager的。通过该CacheManager，使用Cacheable 注解，缓存数据会被放在一个RMap 中，搞清楚这点后，可以比较精准的清除一些key
    @Bean
    CacheManager redissonCacheManager(RedissonClient redissonClient) {
        return new RedissonSpringCacheManager(redissonClient, "classpath:/cache-config.yaml");
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
        Jackson2JsonRedisSerializer<Object> jackson2JsonRedisSerializer = jackson2JsonRedisSerializer();
        // key的序列化采用StringRedisSerializer
        template.setKeySerializer(stringRedisSerializer);
        template.setHashKeySerializer(stringRedisSerializer);
        // value值的序列化采用jackson2JsonRedisSerializer
        template.setValueSerializer(jackson2JsonRedisSerializer);
        template.setHashValueSerializer(jackson2JsonRedisSerializer);

        template.setConnectionFactory(redisConnectionFactory);
        //执行afterPropertiesSet方法，完成属性的设置
        template.afterPropertiesSet();
        return template;
    }


    private Jackson2JsonRedisSerializer<Object> jackson2JsonRedisSerializer() {
        var jackson2JsonRedisSerializer = new Jackson2JsonRedisSerializer<>(Object.class);
        var objectMapper = new ObjectMapper();
        // 如果json中有新增的字段并且是实体类类中不存在的，不报错。即允许json串中有，而pojo中没有的属性
        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        // 指定要序列化的域，field,get和set,以及修饰符范围，ANY是都有包括private和public
        objectMapper.setVisibility(PropertyAccessor.ALL, JsonAutoDetect.Visibility.ANY);
        // enableDefaultTyping 可以认为是序列化是，添加复杂类型的默认类型，以便反序列化时可以精确类型。有下面五种选型
        // - JAVA_LANG_OBJECT：表示将Object类型作为声明类型的属性 设置默认类型。但尽管设置Object，一般都为复杂类型的值，像基本类型和其包装类型的值(String、Boolean、Integer、Double)不会添加默认类型。像自定义对象，List，map等实际对象的都会被添加。
        // - OBJECT_AND_NON_CONCRETE：当属性为Object或非具体类型(抽象类或接口)，但不包括数组类型。当我们定义一个含有接口类的属性时，会设置默认类型
        // - NON_CONCRETE_AND_ARRAYS：OBJECT_AND_NON_CONCRETE值说明了不包含array类型。此属性专门添加了array类型
        // - NON_FINAL：表示将应用与除final修饰外的所有属性，以及所有非final的数组。基本上Object类型和array类型及interface、abstract修饰的类。这些都足以进行序列化。
        // - EVERYTHING：在新版本添加的，如其名
        // 其重载方法包含了两个参数DefaultTyping类型和JsonTypeInfo.As类型
        // JsonTypeInfo.As
        // PROPERTY
        // 它将包含@class属性，作为序列化的一个属性，值就是完全限定名类型。当前类及其属性都会添加这个名为@class的属性。
        // WRAPPER_OBJECT
        // 将完全限定名类型直接作为key，将值作为序列化值
        // WRAPPER_ARRAY
        // 将序列化的结果变为array类型，格式与不加JsonTypeInfo.As一致
        // EXISTING_PROPERTY
        // 仅包含序列化内容，TypeSerializer将不会执行任何操作。与@JsonTypeId注解相似
        //
        // JsonTypeInfo.Id
        // NONE
        // 不包括类型，仅为标准的json序列化值
        // CLASS
        // 使用@class属性标注类型
        // MINIMAL_CLASS
        // 使用@c属性标注类型
        // NAME
        // 使用@type属性标注类型，但只有类名，不是完全限定名，需要将名称单独解析为实际的具体类型（类）。
        // CUSTOM
        //
        // 使用自定义的实现TypeSerializer和TypeDeserializer
        // 必须设置，否则无法将JSON转化为对象，会转化成Map类型。指定序列化输入的类型，类必须是非final修饰的，final修饰的类，比如String,Integer等会抛出异常（使用NON_FINAL时）
        objectMapper.activateDefaultTyping(LaissezFaireSubTypeValidator.instance, ObjectMapper.DefaultTyping.NON_FINAL, JsonTypeInfo.As.PROPERTY);
        jackson2JsonRedisSerializer.setObjectMapper(objectMapper);
        return jackson2JsonRedisSerializer;
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
            return LocalCoreConfig.SYS_NAME + classSimpleName + "::" + methodName + "::" + sha256Hex;
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
