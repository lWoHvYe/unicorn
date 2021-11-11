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

import com.lwohvye.utils.JsonUtils;
import com.lwohvye.utils.serializer.FastJsonRedisSerializer;
import com.lwohvye.utils.serializer.StringRedisSerializer;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
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
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
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
     * 设置@Cacheable 序列化方式
     */
    @Bean
    public RedisCacheConfiguration redisCacheConfiguration() {
        // @Cacheable类的缓存也使用FastJsonRedisSerializer
        FastJsonRedisSerializer<Object> fastJsonRedisSerializer = new FastJsonRedisSerializer<>(Object.class);
        RedisCacheConfiguration configuration = RedisCacheConfiguration.defaultCacheConfig();
        configuration = configuration
                .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(fastJsonRedisSerializer))
                .entryTtl(Duration.ofHours(2));
        return configuration;
    }

    /**
     * @param redisConnectionFactory
     * @return org.springframework.data.redis.core.RedisTemplate
     * @description 与RedisUtil一起使用
     * @date 2021/11/11 1:25 上午
     */
    @Bean(name = "redisTemplate")
    @ConditionalOnMissingBean(name = "redisTemplate")
    public RedisTemplate<Object, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory) {
        var template = new RedisTemplate<>();
        //序列化
        // 2021/11/11 使用Jackson2JsonRedisSerializer时，序列化的结果，在反序列化时会变为Object，丢失类型信息且无法强转成目标的实体。(通过util放置的有包含类型信息,在反序列化时，会自动转回来；通过@Cacheable放置的不行)
        // 具体表现为：Entity序列化后，反序列化时变成Map。无法通过一般方式转回；Map、List丢失范型信息，且List<Entity>变成来List<Map>。🀄️📄就是无法自动转回来。这类可以通过 JsonUtils.toJavaObjectList()转回来，但要转的Entity要有空参构造方法
        // 在使用Redis缓存信息时，对于此类问题不是很好处理（除非每次都缓存前转成Json，缓存后再取出来，J2B转回原实体），故此处继续使用FastJson。
        var fastJsonRedisSerializer = new FastJsonRedisSerializer<>(Object.class);
        // value值的序列化采用fastJsonRedisSerializer
        template.setValueSerializer(fastJsonRedisSerializer);
        template.setHashValueSerializer(fastJsonRedisSerializer);
        //当一个类中包含了一个接口（或抽象类）的时候，在使用fastjson进行序列化的时候，会将子类型抹去，只保留接口（抽象类）的类型，使得反序列化时无法拿到原始类型。
        //为了解决这个问题呢，fastjson引入了AutoType，即在序列化的时候，把原始类型记录下来。
        // 全局开启AutoType，这里方便开发，使用全局的方式 https://github.com/alibaba/fastjson/wiki/enable_autotype
//        ParserConfig.getGlobalInstance().setAutoTypeSupport(true);
        // 建议使用这种方式，小范围指定白名单
//        var parserConfig = ParserConfig.getGlobalInstance();
//        parserConfig.addAccept("com.lwohvye.domain");
//        parserConfig.addAccept("com.lwohvye.modules.");
        // 开启safeMode https://github.com/alibaba/fastjson/wiki/fastjson_safemode
//        ParserConfig.getGlobalInstance().setSafeMode(true);
        // 示例-autoTypeCheckHandler的添加。非safeMode模式下，不要开启下面的配置
//        ParserConfig.getGlobalInstance().addAutoTypeCheckHandler(new GrantedAuthorityAutoTypeCheckHandler());
        // 亦可使用Jackson2JsonRedisSerializer来序列化和反序列化redis的value值
//        var jackson2JsonRedisSerializer = new Jackson2JsonRedisSerializer<>(Object.class);
//        var objectMapper = new ObjectMapper();
//        objectMapper.setVisibility(PropertyAccessor.ALL, JsonAutoDetect.Visibility.ANY);
//        objectMapper.activateDefaultTyping(LaissezFaireSubTypeValidator.instance, ObjectMapper.DefaultTyping.NON_FINAL);
//        jackson2JsonRedisSerializer.setObjectMapper(objectMapper);
//
//        template.setValueSerializer(jackson2JsonRedisSerializer);
//        template.setHashValueSerializer(jackson2JsonRedisSerializer);

        // key的序列化采用StringRedisSerializer
        template.setKeySerializer(new StringRedisSerializer());
        template.setHashKeySerializer(new StringRedisSerializer());
        template.setConnectionFactory(redisConnectionFactory);
        //执行afterPropertiesSet方法，完成属性的设置
        template.afterPropertiesSet();
        return template;
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
// https://github.com/alibaba/fastjson/wiki/fastjson_safemode
// 这部分未用到，仅作为示例使用
//public class GrantedAuthorityAutoTypeCheckHandler implements ParserConfig.AutoTypeCheckHandler {
//
//    public Class<?> handler(String typeName, Class<?> expectClass, int features) {
//        return switch (typeName) {
//            case "JaasGrantedAuthority" -> JaasGrantedAuthority.class;
//            case "SimpleGrantedAuthority" -> SimpleGrantedAuthority.class;
//            case "SwitchUserGrantedAuthority" -> SwitchUserGrantedAuthority.class;
//            default -> GrantedAuthority.class;
//        };
//    }
//}
