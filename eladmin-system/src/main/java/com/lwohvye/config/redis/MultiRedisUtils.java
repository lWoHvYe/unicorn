package com.lwohvye.config.redis;

import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.utils.RedisUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

@Component
public class MultiRedisUtils extends RedisUtils {

    @Autowired
    @Qualifier(value = "main2RedisTemplate")
    private RedisTemplate<Object, Object> main2RedisTemplate;

    public MultiRedisUtils(RedisTemplate<Object, Object> redisTemplate) {
        super(redisTemplate);
    }

    //  构造方法  ——> @Autowired —— > @PostConstruct ——> 静态方法 （按此顺序加载）
    @PostConstruct
    public void init() {
//        为父类的该属性重新赋值。注意该属性需设置为public
        super.redisTemplate = main2RedisTemplate;
    }
}
