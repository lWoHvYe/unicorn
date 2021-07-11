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

import com.lwohvye.utils.redis.RedisUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * RedisUtils的一个子类。使用另一个Redis数据源。从
 */
@Component
public class SlaveRedisUtils extends RedisUtils {

    @Autowired
    @Qualifier(value = "slaveRedisTemplate")
    private RedisTemplate<Object, Object> authSlaveRedisTemplate;

    public SlaveRedisUtils(RedisTemplate<Object, Object> redisTemplate) {
        super(redisTemplate);
    }

    //  构造方法  ——> @Autowired —— > @PostConstruct ——> 静态方法 （按此顺序加载）
    @PostConstruct
    public void init() {
//        为父类的该属性重新赋值。注意该属性需设置为public/protected
        super.redisTemplate = authSlaveRedisTemplate;
    }
}
