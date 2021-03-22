package com.lwohvye.config.redis;

import com.lwohvye.utils.RedisUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * RedisUtils的一个子类。使用另一个Redis数据源。做单点登陆。存储token。从
 */
@Component
public class AuthSlaveRedisUtils extends RedisUtils {

    @Autowired
    @Qualifier(value = "authSlaveRedisTemplate")
    private RedisTemplate<Object, Object> authSlaveRedisTemplate;

    public AuthSlaveRedisUtils(RedisTemplate<Object, Object> redisTemplate) {
        super(redisTemplate);
    }

    //  构造方法  ——> @Autowired —— > @PostConstruct ——> 静态方法 （按此顺序加载）
    @PostConstruct
    public void init() {
//        为父类的该属性重新赋值。注意该属性需设置为public/protected
        super.redisTemplate = authSlaveRedisTemplate;
    }
}
