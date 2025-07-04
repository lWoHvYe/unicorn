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
package com.lwohvye.core.utils.redis;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import com.lwohvye.core.constant.LocalCoreConstant;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.DataType;
import org.springframework.data.redis.core.*;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.util.Assert;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.LockSupport;

/**
 * Redis相关工具类。
 * 因为兼容多数据源。当前需配合system模块中的相关配置类一起使用，需注意
 *
 * @author lWoHvYe
 */
@SuppressWarnings({"unchecked", "rawtypes", "unused"})
public class RedisUtils {
    private static final Logger log = LoggerFactory.getLogger(RedisUtils.class);
    //    允许同一包下，及子类访问
    protected RedisTemplate<Object, Object> redisTemplate;

    // Redis lua的执行出错，是因为Redis的Value的序列化使用的Json相关的。针对lua相关的操作，可以使用StringRedisTemplate
    protected StringRedisTemplate stringRedisTemplate;

    protected RedissonClient redissonClient;

    //    分布式锁前缀
    private static final String DISTRIBUTE_LOCK_PREFIX = "redis-lock-";

    //    分布式锁失效时间
    private static final Long DISTRIBUTE_LOCK_EXPIRE = 200_000L;

    public RedisUtils(RedisTemplate<Object, Object> redisTemplate, StringRedisTemplate stringRedisTemplate, RedissonClient redissonClient) {
        this.redisTemplate = redisTemplate;
        this.stringRedisTemplate = stringRedisTemplate;
        this.redissonClient = redissonClient;
    }

    // https://github.com/redisson/redisson/wiki/11.-redis%E5%91%BD%E4%BB%A4%E5%92%8Credisson%E5%AF%B9%E8%B1%A1%E5%8C%B9%E9%85%8D%E5%88%97%E8%A1%A8      慢慢将其变成Redisson的形状

//    region key相关操作

    /**
     * 删除key
     *
     * @param key
     */
    public void delete(String key) {
        redisTemplate.delete(key);
    }

    /**
     * 删除缓存
     *
     * @param keys 可以传一个值 或多个
     */
    public void delete(String... keys) {
        if (keys != null && keys.length > 0) {
            if (keys.length == 1) {
                boolean result = Boolean.TRUE.equals(redisTemplate.delete(keys[0]));
                log.debug("删除缓存：{} ，结果：{} ", keys[0], result);
            } else {
                Set<Object> keySet = new HashSet<>();
                for (String key : keys) {
                    keySet.addAll(Objects.requireNonNull(redisTemplate.keys(key)));
                }
                Long count = redisTemplate.delete(keySet);
                log.debug("成功删除缓存：{}", keySet);
                log.debug("缓存删除数量：{} 个", count);
            }
        }
    }

    /**
     * 指定缓存失效时间。在指定长的时间之后过期
     *
     * @param key  键
     * @param time 时间(秒)
     */
    public boolean expire(String key, long time) {
        try {
            if (time > 0) {
                redisTemplate.expire(key, time, TimeUnit.SECONDS);
            }
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
        return true;
    }

    /**
     * 指定缓存失效时间
     *
     * @param key      键
     * @param time     时间(秒)
     * @param timeUnit 单位
     */
    public boolean expire(String key, long time, TimeUnit timeUnit) {
        try {
            if (time > 0) {
                redisTemplate.expire(key, time, timeUnit);
            }
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
        return true;
    }

    /**
     * 设置过期时间。在指定的时刻过期
     *
     * @param key
     * @param date
     * @return
     */
    public Boolean expireAt(String key, Date date) {
        return redisTemplate.expireAt(key, date);
    }

    /**
     * 返回 key 的剩余的过期时间
     *
     * @param key
     * @return
     */
    public Long getExpire(String key) {
        return redisTemplate.getExpire(key);
    }

    /**
     * 根据 key 获取过期时间
     *
     * @param key 键 不能为null
     * @return 时间(秒) 返回0代表为永久有效
     */
    public Long getExpire(Object key) {
        return redisTemplate.getExpire(key, TimeUnit.SECONDS);
    }

    /**
     * 修改 key 的名称
     *
     * @param oldKey
     * @param newKey
     */
    public void rename(String oldKey, String newKey) {
        redisTemplate.rename(oldKey, newKey);
    }

    /**
     * 仅当 newkey 不存在时，将 oldKey 改名为 newkey
     *
     * @param oldKey
     * @param newKey
     * @return
     */
    public Boolean renameIfAbsent(String oldKey, String newKey) {
        return redisTemplate.renameIfAbsent(oldKey, newKey);
    }

    /**
     * 查找匹配key
     *
     * @param pattern key
     * @return /
     */
    public List<String> scan(String pattern) {
        var options = ScanOptions.scanOptions().match(pattern).build();
        var factory = redisTemplate.getConnectionFactory();
        var rc = Objects.requireNonNull(factory).getConnection();
        List<String> result = new ArrayList<>();
        try (var cursor = rc.keyCommands().scan(options)) {
            while (cursor.hasNext()) {
                result.add(new String(cursor.next()));
            }
        }
        try {
            RedisConnectionUtils.releaseConnection(rc, factory);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        return result;
    }

    /**
     * 返回 key 所储存的值的类型
     *
     * @param key
     * @return
     */
    public DataType type(String key) {
        return redisTemplate.type(key);
    }

    /**
     * 分页查询 key
     *
     * @param patternKey key
     * @param page       页码
     * @param size       每页数目
     * @return /
     */
    public List<String> findKeysForPage(String patternKey, int page, int size) {
        var options = ScanOptions.scanOptions().match(patternKey).build();
        var factory = redisTemplate.getConnectionFactory();
        var rc = Objects.requireNonNull(factory).getConnection();
        List<String> result = new ArrayList<>(size);
        try (var cursor = rc.keyCommands().scan(options)) {
            int tmpIndex = 0;
            int fromIndex = page * size;
            int toIndex = page * size + size;
            while (cursor.hasNext()) {
                if (tmpIndex >= fromIndex && tmpIndex < toIndex) {
                    result.add(new String(cursor.next()));
                    tmpIndex++;
                    continue;
                }
                // 还未获取到满足条件的数据,继续
                if (tmpIndex < toIndex) {
                    tmpIndex++;
                    cursor.next();
                }
            }
        }
        try {
            RedisConnectionUtils.releaseConnection(rc, factory);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        return result;
    }

    /**
     * 判断key是否存在
     *
     * @param key 键
     * @return true 存在 false不存在
     */
    public boolean hasKey(String key) {
        try {
            return Boolean.TRUE.equals(redisTemplate.hasKey(key));
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

//    endregion

//    region string相关操作

    /**
     * 普通缓存放入
     *
     * @param key   键
     * @param value 值
     * @return true成功 false失败
     */
    public boolean set(String key, Object value) {
        try {
            redisTemplate.opsForValue().set(key, value);
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

//    private static String luaCpMoreScript =
//            """
//                    if redis.call('exists',KEYS[1]) == 0 or redis.call('get',KEYS[1]) < ARGV[1] then
//                       return redis.call('set',KEYS[1],ARGV[1])
//                    else
//                       return 0
//                    end
//                    """;
//    private RedisScript<String> cpMoreRedisScript = new DefaultRedisScript<>(luaCpMoreScript, String.class);
//
//    /**
//     * @param key
//     * @param value
//     * @return boolean
//     * 比原值大时再更新。true表示进行了更新。但redis的比较是string维度的，所以废弃
//     * @date 2021/7/17 9:59
//     */
//    public boolean setIfLarger(String key, Long value) {
//        if (StrUtil.isEmpty(key))
//            throw new RuntimeException("key不可为空");
//        Object result = redisTemplate.execute(cpMoreRedisScript, Collections.singletonList(key), value);
//        return SUCCESS.equals(result);
//    }

    /**
     * 普通缓存放入并设置时间
     *
     * @param key   键
     * @param value 值
     * @param time  时间(秒) time要大于0 如果time小于等于0 将设置无限期
     * @return true成功 false 失败
     */
    public boolean set(String key, Object value, long time) {
        try {
            if (time > 0) {
                redisTemplate.opsForValue().set(key, value, time, TimeUnit.SECONDS);
            } else {
                set(key, value);
            }
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 普通缓存放入并设置时间
     *
     * @param key      键
     * @param value    值
     * @param time     时间
     * @param timeUnit 类型
     * @return true成功 false 失败
     */
    public boolean set(String key, Object value, long time, TimeUnit timeUnit) {
        try {
            if (time > 0) {
                redisTemplate.opsForValue().set(key, value, time, timeUnit);
            } else {
                set(key, value);
            }
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 普通缓存获取
     *
     * @param key 键
     * @return 值
     */
    public Object get(String key) {
        return key == null ? null : redisTemplate.opsForValue().get(key);
    }

    //SET_IF_PRESENT --->NX
    //SET_IF_ABSENT   ---->XX
    //NX – Only set the key if it does not already exist.
    //XX – Only set the key if it already exist.

    //EX second ：设置键的过期时间为 second 秒。 SET key value EX second 效果等同于 SETEX key second value 。
    //PX millisecond ：设置键的过期时间为 millisecond 毫秒。 SET key value PX millisecond 效果等同于 PSETEX key millisecond value 。
    //NX ：只在键不存在时，才对键进行设置操作。 SET key value NX 效果等同于 SETNX key value 。
    //XX ：只在键已经存在时，才对键进行设置操作。

    /**
     * 只有在 key 不存在时设置 key 的值
     *
     * @param key
     * @param value
     * @return 之前已经存在返回false, 不存在返回true
     */
    public boolean setIfAbsent(String key, Object value) {
        return Boolean.TRUE.equals(redisTemplate.opsForValue().setIfAbsent(key, value));
    }

    /**
     * 只有在key存在时，才更新key的值
     *
     * @param key
     * @param value
     * @return boolean
     * @date 2021/7/11 0:56
     */
    public boolean setIfPresent(String key, Object value) {
        return Boolean.TRUE.equals(redisTemplate.opsForValue().setIfPresent(key, value));
    }

    /**
     * 只有在Key不存在时，设置其值
     *
     * @param key
     * @param value
     * @param time
     * @param timeUnit
     * @return java.lang.Boolean
     * @date 2021/7/11 0:49
     */
    public Boolean setIfAbsent(String key, Object value, long time, TimeUnit timeUnit) {
        Boolean result;
        try {
            if (ObjectUtil.isNotNull(time) && time > 0) {
                result = redisTemplate.opsForValue().setIfAbsent(key, value, time, timeUnit);
            } else {
                result = setIfAbsent(key, value);
            }
            return result;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 只有在key存在时，更新其属性
     *
     * @param key
     * @param value
     * @param time
     * @param timeUnit
     * @return java.lang.Boolean
     * @date 2021/7/11 0:59
     */
    public Boolean setIfPresent(String key, Object value, long time, TimeUnit timeUnit) {
        Boolean result;
        try {
            if (ObjectUtil.isNotNull(time) && time > 0) {
                result = redisTemplate.opsForValue().setIfPresent(key, value, time, timeUnit);
            } else {
                result = setIfPresent(key, value);
            }
            return result;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 获取字符串的长度
     *
     * @param key
     * @return
     */
    public Long size(String key) {
        return redisTemplate.opsForValue().size(key);
    }

    /**
     * 增加(自增长), 负数则为自减
     *
     * @param key
     * @param increment
     * @return
     */
    public Long incrBy(String key, long increment) {
        return redisTemplate.opsForValue().increment(key, increment);
    }

    /**
     * 追加到末尾
     *
     * @param key
     * @param value
     * @return
     */
    public Integer append(String key, String value) {
        return redisTemplate.opsForValue().append(key, value);
    }

//    endregion

//    region hash相关操作

    /**
     * 获取存储在哈希表中指定字段的值
     *
     * @param key
     * @param item
     * @return
     */
    public Object hGet(String key, String item) {
        return redisTemplate.opsForHash().get(key, item);
    }

    /**
     * 向一张hash表中放入数据,如果不存在将创建
     *
     * @param key   键
     * @param item  项
     * @param value 值
     * @return true 成功 false失败
     */
    public Boolean hPut(String key, String item, Object value) {
        try {
            redisTemplate.opsForHash().put(key, item, value);
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 向一张hash表中放入数据,如果不存在将创建
     *
     * @param key   键
     * @param item  项
     * @param value 值
     * @param time  时间(秒) 注意:如果已存在的hash表有时间,这里将会替换原有的时间
     * @return true 成功 false失败
     */
    public Boolean hPut(String key, String item, Object value, long time) {
        try {
            redisTemplate.opsForHash().put(key, item, value);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * HashSet
     *
     * @param key 键
     * @param map 对应多个键值
     * @return true 成功 false 失败
     */
    public Boolean hPutAll(String key, Map<String, Object> map) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * HashSet 并设置时间
     *
     * @param key  键
     * @param map  对应多个键值
     * @param time 时间(秒)
     * @return true成功 false失败
     */
    public Boolean hPutAll(String key, Map<String, Object> map, long time) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 删除一个或多个哈希表字段
     *
     * @param key
     * @param items
     * @return
     */
    public Long hDelete(String key, Object... items) {
        return redisTemplate.opsForHash().delete(key, items);
    }

    /**
     * 判断hash表中是否有该项的值
     *
     * @param key  键 不能为null
     * @param item 项 不能为null
     * @return true 存在 false不存在
     */
    public boolean hHasKey(String key, String item) {
        return redisTemplate.opsForHash().hasKey(key, item);
    }

    /**
     * 查看哈希表 key 中，指定的字段是否存在
     *
     * @param key
     * @param item
     * @return
     */
    public boolean hExists(String key, String item) {
        return redisTemplate.opsForHash().hasKey(key, item);
    }

    // 因为配置了value的序列化为json。所以自增类操作将无法进行

    /**
     * 为哈希表 key 中的指定字段的整数值加上增量 increment
     * 传负值时减去增量
     *
     * @param key
     * @param field
     * @param increment
     * @return
     */
    public Long hIncrBy(String key, Object field, long increment) {
        return redisTemplate.opsForHash().increment(key, field, increment);
    }

    /**
     * 获取所有哈希表中的字段
     *
     * @param key
     * @return
     */
    public Set<Object> hKeys(String key) {
        return redisTemplate.opsForHash().keys(key);
    }

    /**
     * 获取哈希表中字段的数量
     *
     * @param key
     * @return
     */
    public Long hSize(String key) {
        return redisTemplate.opsForHash().size(key);
    }

    /**
     * 获取哈希表中所有值
     *
     * @param key
     * @return
     */
    public List<Object> hValues(String key) {
        return redisTemplate.opsForHash().values(key);
    }

    //    endregion

//    region list相关操作

    /**
     * 通过索引 获取list中的值
     *
     * @param key   键
     * @param index 索引 index不小于0时， 0 表头，1 第二个元素，依次类推；index小于0时，-1，表尾，-2倒数第二个元素，依次类推
     * @return
     */
    public Object lGetByIndex(String key, long index) {
        try {
            return redisTemplate.opsForList().index(key, index);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return null;
        }
    }

    /**
     * 获取list缓存的内容
     *
     * @param key   键
     * @param start 开始
     * @param end   结束 0 到 -1代表所有值
     * @return
     */
    public List<Object> lGet(String key, long start, long end) {
        try {
            return redisTemplate.opsForList().range(key, start, end);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return null;
        }
    }

    /**
     * 存储在list头部
     *
     * @param key
     * @param value
     * @return
     */
    public Long lLeftPush(String key, Object value) {
        return redisTemplate.opsForList().leftPush(key, value);
    }

    /**
     * @param key
     * @param value
     * @return
     */
    public Long lRightPush(String key, Object value) {
        return redisTemplate.opsForList().rightPush(key, value);
    }

    /**
     * 通过索引设置列表元素的值
     *
     * @param key
     * @param index 位置
     * @param value
     */
    public void lSetByIndex(String key, long index, Object value) {
        redisTemplate.opsForList().set(key, index, value);
    }

    /**
     * 移出并获取列表的第一个元素
     *
     * @param key
     * @return 删除的元素
     */
    public Object lLeftPop(String key) {
        return redisTemplate.opsForList().leftPop(key);
    }

    /**
     * 移出并获取列表的第一个元素， 如果列表没有元素会阻塞列表直到等待超时或发现可弹出元素为止
     *
     * @param key
     * @param timeout 等待时间
     * @param unit    时间单位
     * @return
     */
    public Object lBLeftPop(String key, long timeout, TimeUnit unit) {
        return redisTemplate.opsForList().leftPop(key, timeout, unit);
    }

    /**
     * 移除并获取列表最后一个元素
     *
     * @param key
     * @return 删除的元素
     */
    public Object lRightPop(String key) {
        return redisTemplate.opsForList().rightPop(key);
    }

    /**
     * 移出并获取列表的最后一个元素， 如果列表没有元素会阻塞列表直到等待超时或发现可弹出元素为止
     *
     * @param key
     * @param timeout 等待时间
     * @param unit    时间单位
     * @return
     */
    public Object lBRightPop(String key, long timeout, TimeUnit unit) {
        return redisTemplate.opsForList().rightPop(key, timeout, unit);
    }

    /**
     * 删除集合中值等于value得元素
     *
     * @param key
     * @param count count等于0, 删除所有值等于value的元素; count大于0, 从头部开始删除第一个值等于value的元素;
     *              count小于0, 从尾部开始删除第一个值等于value的元素;
     * @param value
     * @return
     */
    public Long lRemove(String key, long count, Object value) {
        try {
            return redisTemplate.opsForList().remove(key, count, value);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return 0L;
        }
    }

    /**
     * 裁剪list
     *
     * @param key
     * @param start
     * @param end
     */
    public void lTrim(String key, long start, long end) {
        redisTemplate.opsForList().trim(key, start, end);
    }

    /**
     * 获取list缓存的长度
     *
     * @param key 键
     * @return
     */
    public Long lSize(String key) {
        try {
            return redisTemplate.opsForList().size(key);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return 0L;
        }
    }

    //    endregion

//    region set相关操作

    /**
     * 将数据放入set缓存
     *
     * @param key    键
     * @param values 值 可以是多个
     * @return 成功个数
     */
    // @SafeVarargs 读取的时候可以用这个限制警告，当前无用。作用等同于SuppressWamings("unchecked"。)
    public Long sSet(String key, Object... values) {
        try {
            return redisTemplate.opsForSet().add(key, values);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return 0L;
        }
    }

    /**
     * 将set数据放入缓存
     *
     * @param key    键
     * @param time   时间(秒)
     * @param values 值 可以是多个
     * @return 成功个数
     */
    public long sSet(String key, long time, Object... values) {
        try {
            Long count = redisTemplate.opsForSet().add(key, values);
            if (time > 0) {
                expire(key, time);
            }
            return count;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return 0;
        }
    }

    /**
     * set移除元素，移除值为value的
     *
     * @param key
     * @param values
     * @return
     */
    public Long sRemove(String key, Object... values) {
        try {
            return redisTemplate.opsForSet().remove(key, values);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return 0L;
        }
    }

    /**
     * 移除并返回集合的一个随机元素
     *
     * @param key
     * @return
     */
    public Object sPop(String key) {
        return redisTemplate.opsForSet().pop(key);
    }

    /**
     * 将元素value从一个集合移到另一个集合
     *
     * @param key
     * @param value
     * @param destKey
     * @return
     */
    public Boolean sMove(String key, String value, String destKey) {
        return redisTemplate.opsForSet().move(key, value, destKey);
    }

    /**
     * 获取集合的大小
     *
     * @param key
     * @return
     */
    public Long sSize(String key) {
        try {
            return redisTemplate.opsForSet().size(key);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return 0L;
        }
    }

    /**
     * 判断集合是否包含value
     *
     * @param key
     * @param value
     * @return
     */
    public Boolean sIsMember(String key, Object value) {
        return redisTemplate.opsForSet().isMember(key, value);
    }

    /**
     * 根据value从一个set中查询,是否存在
     *
     * @param key   键
     * @param value 值
     * @return true 存在 false不存在
     */
    public boolean sHasKey(String key, Object value) {
        try {
            return Boolean.TRUE.equals(redisTemplate.opsForSet().isMember(key, value));
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    /**
     * 获取两个集合的交集
     *
     * @param key
     * @param otherKey
     * @return
     */
    public Set<Object> sIntersect(String key, String otherKey) {
        return redisTemplate.opsForSet().intersect(key, otherKey);
    }

    /**
     * 获取key集合与多个集合的交集
     *
     * @param key
     * @param otherKeys
     * @return
     */
    public Set<Object> sIntersect(String key, Collection<String> otherKeys) {
        return redisTemplate.opsForSet().intersect(key, otherKeys);
    }

    /**
     * 获取两个集合的并集
     *
     * @param key
     * @param otherKeys
     * @return
     */
    public Set<Object> sUnion(String key, String otherKeys) {
        return redisTemplate.opsForSet().union(key, otherKeys);
    }

    /**
     * 获取key集合与多个集合的并集
     *
     * @param key
     * @param otherKeys
     * @return
     */
    public Set<Object> sUnion(String key, Collection<String> otherKeys) {
        return redisTemplate.opsForSet().union(key, otherKeys);
    }

    /**
     * 获取两个集合的差集
     *
     * @param key
     * @param otherKey
     * @return
     */
    public Set<Object> sDifference(String key, String otherKey) {
        return redisTemplate.opsForSet().difference(key, otherKey);
    }

    /**
     * 获取key集合与多个集合的差集
     *
     * @param key
     * @param otherKeys
     * @return
     */
    public Set<Object> sDifference(String key, Collection<String> otherKeys) {
        return redisTemplate.opsForSet().difference(key, otherKeys);
    }

    /**
     * 获取集合所有元素
     *
     * @param key
     */
    public Set<Object> setMembers(String key) {
        try {
            return redisTemplate.opsForSet().members(key);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return Collections.emptySet();
        }
    }

    /**
     * 随机获取集合中的一个元素
     *
     * @param key
     */
    public Object sRandomMember(String key) {
        return redisTemplate.opsForSet().randomMember(key);
    }

    /**
     * @param key
     * @param options
     */
    public Cursor<Object> sScan(String key, ScanOptions options) {
        return redisTemplate.opsForSet().scan(key, options);
    }

//    endregion

//    region zSet相关操作

    /**
     * 添加元素,有序集合是按照元素的score值由小到大排列
     *
     * @param key
     * @param value
     * @param score
     */
    public Boolean zAdd(String key, Object value, double score) {
        return redisTemplate.opsForZSet().add(key, value, score);
    }

    /**
     * 添加元素，key不存在则添加，存在则不操作
     *
     * @param key
     * @param value
     * @param score
     * @return java.lang.Boolean
     * @date 2021/7/11 0:35
     */
    public Boolean zAddIfAbsent(String key, Object value, double score) {
        return redisTemplate.opsForZSet().addIfAbsent(key, value, score);
    }

    /**
     * @param key
     * @param values
     */
    public Long zAdd(String key, Set<ZSetOperations.TypedTuple<Object>> values) {
        return redisTemplate.opsForZSet().add(key, values);
    }

    /**
     * 添加元素，key并不存在则添加，存在则不操作
     *
     * @param key
     * @param values
     * @return java.lang.Long
     * @date 2021/7/11 0:37
     */
    public Long zAddIfAbsent(String key, Set<ZSetOperations.TypedTuple<Object>> values) {
        return redisTemplate.opsForZSet().addIfAbsent(key, values);
    }

    /**
     * 移除元素
     *
     * @param key
     * @param values
     */
    public Long zRemove(String key, Object... values) {
        return redisTemplate.opsForZSet().remove(key, values);
    }

    /**
     * 增加元素的score值，并返回增加后的值
     *
     * @param key
     * @param value
     * @param delta
     */
    public Double zIncrementScore(String key, Object value, double delta) {
        return redisTemplate.opsForZSet().incrementScore(key, value, delta);
    }

    /**
     * 返回元素在集合的排名,有序集合是按照元素的score值由小到大排列
     *
     * @param key
     * @param value
     * @return 0表示第一位
     */
    public Long zRank(String key, Object value) {
        return redisTemplate.opsForZSet().rank(key, value);
    }

    /**
     * 返回元素在集合的排名,按元素的score值由大到小排列
     *
     * @param key
     * @param value
     * @return
     */
    public Long zReverseRank(String key, Object value) {
        return redisTemplate.opsForZSet().reverseRank(key, value);
    }

    /**
     * 获取集合的元素, 从小到大排序
     *
     * @param key
     * @param start 开始位置
     * @param end   结束位置, -1查询所有
     * @return
     */
    public Set<Object> zRange(String key, long start, long end) {
        return redisTemplate.opsForZSet().range(key, start, end);
    }

    /**
     * 获取集合元素, 并且把score值也获取
     * 返回的Set是有序的LinkedHashSet
     * @param key
     * @param start
     * @param end
     * @return
     */
    public Set<ZSetOperations.TypedTuple<Object>> zRangeWithScores(String key, long start, long end) {
        return redisTemplate.opsForZSet().rangeWithScores(key, start, end);
    }

    /**
     * 根据Score值查询集合元素
     *
     * @param key
     * @param min 最小值
     * @param max 最大值
     * @return
     */
    public Set<Object> zRangeByScore(String key, double min, double max) {
        return redisTemplate.opsForZSet().rangeByScore(key, min, max);
    }

    /**
     * 根据Score值查询集合元素, 从小到大排序
     *
     * @param key
     * @param min 最小值
     * @param max 最大值
     * @return
     */
    public Set<ZSetOperations.TypedTuple<Object>> zRangeByScoreWithScores(String key, double min, double max) {
        return redisTemplate.opsForZSet().rangeByScoreWithScores(key, min, max);
    }

    /**
     * @param key
     * @param min
     * @param max
     * @param start
     * @param end
     * @return
     */
    public Set<ZSetOperations.TypedTuple<Object>> zRangeByScoreWithScores(String key, double min, double max, long start, long end) {
        return redisTemplate.opsForZSet().rangeByScoreWithScores(key, min, max, start, end);
    }

    /**
     * 获取集合的元素, 从大到小排序
     *
     * @param key
     * @param start
     * @param end
     * @return
     */
    public Set<Object> zReverseRange(String key, long start, long end) {
        return redisTemplate.opsForZSet().reverseRange(key, start, end);
    }

    /**
     * 获取集合的元素, 从大到小排序, 并返回score值
     *
     * @param key
     * @param start
     * @param end
     * @return
     */
    public Set<ZSetOperations.TypedTuple<Object>> zReverseRangeWithScores(String key, long start, long end) {
        return redisTemplate.opsForZSet().reverseRangeWithScores(key, start, end);
    }

    /**
     * 根据Score值查询集合元素, 从大到小排序
     *
     * @param key
     * @param min
     * @param max
     * @return
     */
    public Set<Object> zReverseRangeByScore(String key, double min, double max) {
        return redisTemplate.opsForZSet().reverseRangeByScore(key, min, max);
    }

    /**
     * 根据Score值查询集合元素, 从大到小排序
     *
     * @param key
     * @param min
     * @param max
     * @return
     */
    public Set<ZSetOperations.TypedTuple<Object>> zReverseRangeByScoreWithScores(String key, double min, double max) {
        return redisTemplate.opsForZSet().reverseRangeByScoreWithScores(key, min, max);
    }

    /**
     * 根据score值获取集合元素数量
     *
     * @param key
     * @param min
     * @param max
     * @return
     */
    public Long zCount(String key, double min, double max) {
        return redisTemplate.opsForZSet().count(key, min, max);
    }

    /**
     * 获取集合大小
     *
     * @param key
     * @return
     */
    public Long zSize(String key) {
        return redisTemplate.opsForZSet().size(key);
    }

    /**
     * 获取集合大小
     *
     * @param key
     * @return
     */
    public Long zZCard(String key) {
        return redisTemplate.opsForZSet().zCard(key);
    }

    /**
     * 获取集合中value元素的score值
     *
     * @param key
     * @param value
     * @return
     */
    public Double zScore(String key, Object value) {
        return redisTemplate.opsForZSet().score(key, value);
    }

    /**
     * 移除指定索引位置的成员
     *
     * @param key
     * @param start
     * @param end
     * @return
     */
    public Long zRemoveRange(String key, long start, long end) {
        return redisTemplate.opsForZSet().removeRange(key, start, end);
    }

    /**
     * 根据指定的score值的范围来移除成员
     *
     * @param key
     * @param min
     * @param max
     * @return
     */
    public Long zRemoveRangeByScore(String key, double min, double max) {
        return redisTemplate.opsForZSet().removeRangeByScore(key, min, max);
    }

    /**
     * @param key
     * @param options
     * @return
     */
    public Cursor<ZSetOperations.TypedTuple<Object>> zScan(String key, ScanOptions options) {
        return redisTemplate.opsForZSet().scan(key, options);
    }


    // key不存在 member不存在 原分数比新分数低（长度+比较，非负）
    // Redis中的不存在nil，对应lua中的false
    // ZADD命令，返回被成功添加的新成员的数量，不包括那些被更新的、已经存在的成员
    private static final String LUA_HG_SCORE_SCRIPT =
            """
                    local rsc = redis.call('ZSCORE',KEYS[1],ARGV[2])
                    if ( not rsc ) or ( tonumber(rsc) < tonumber(ARGV[1]) ) then
                       redis.call('ZADD',KEYS[1],ARGV[1],ARGV[2])
                       return 1
                    else
                       return 0
                    end
                    """;
    private final RedisScript<Long> hgScoreRedisScript = new DefaultRedisScript<>(LUA_HG_SCORE_SCRIPT, Long.class);

    /**
     * 当本次分数比Redis中高时，再更新。
     *
     * @param key
     * @param value
     * @param score
     * @return boolean
     * @date 2021/7/17 11:52
     */
    //  当value类型为String时 这里存入的value为 001，通过其他方法存入的为 "001"，是不一样的，这点需特别注意，要想保持一致，lua相关的需要手动在前后拼双引号    "\"" + value + "\""
    //  当value类型为long时，lua存入的值不带L，但通过其他方法存入的带L。其他类型的也同理，需要进行处理
    // lua相关的，需要注意引号的问题
    public boolean zAddIfHigherScore(String key, Object value, double score) {
        Assert.state(StrUtil.isNotEmpty(key) && ObjectUtil.isNotNull(value) && ObjectUtil.isNotNull(score), "参数不合法");
        if (value instanceof String)
            value = "\"" + value + "\"";
        // 用StringRedisTemplate，则k v 都要是String
        Object result = stringRedisTemplate.execute(hgScoreRedisScript, Collections.singletonList(key), String.valueOf(score), String.valueOf(value));
        return SUCCESS.equals(result);
    }

    private static final String LUA_LW_SCORE_SCRIPT =
            """
                    local rsc = redis.call('ZSCORE',KEYS[1],ARGV[2])
                    if ( not rsc ) or ( tonumber(rsc) > tonumber(ARGV[1]) ) then
                       redis.call('ZADD',KEYS[1],ARGV[1],ARGV[2])
                       return 1
                    else
                       return 0
                    end
                    """;
    private final RedisScript<Long> lwScoreRedisScript = new DefaultRedisScript<>(LUA_LW_SCORE_SCRIPT, Long.class);

    public boolean zAddIfLowerScore(String key, Object value, double score) {
        Assert.state(StrUtil.isNotEmpty(key) && ObjectUtil.isNotNull(value) && ObjectUtil.isNotNull(score), "参数不合法");
        if (value instanceof String)
            value = "\"" + value + "\"";
        Object result = stringRedisTemplate.execute(lwScoreRedisScript, Collections.singletonList(key), String.valueOf(score), String.valueOf(value));
        return SUCCESS.equals(result);
    }
//       endregion

    /**
     * 删除通过Redisson存入的key
     *
     * @param map /
     * @param suf 标识
     * @date 2022/3/12 12:03 AM
     */
    public void delInRC(Map<String, String> map, Object suf) {
        var mapCache = redissonClient.getMapCache(map.get(LocalCoreConstant.CACHE_NAME));
        var prefix = map.get(LocalCoreConstant.CACHE_KEY);
        mapCache.remove(Objects.nonNull(suf) ? prefix + suf : prefix);
    }

    /**
     * @param map 前缀
     * @param ids id
     */
    public void delByKeys4Business(Map<String, String> map, Set<Long> ids) {
        // Set<Object> keys = new HashSet<>();
        var mapCache = redissonClient.getMapCache(map.get(LocalCoreConstant.CACHE_NAME));
        var prefix = map.get(LocalCoreConstant.CACHE_KEY);
        for (Long id : ids)
            mapCache.remove(prefix + id);
    }

    /**
     * 最终加强分布式锁
     *
     * @param key key值
     * @return 是否获取到
     */
    public Boolean lock(String key) {
        String lock = DISTRIBUTE_LOCK_PREFIX + key;
        // 利用lambda表达式
        return (Boolean) redisTemplate.execute((RedisCallback<Object>) redisConnection -> {
            long expireAt = System.currentTimeMillis() + DISTRIBUTE_LOCK_EXPIRE + 1;
            Boolean acquire = redisConnection.stringCommands().setNX(lock.getBytes(), String.valueOf(expireAt).getBytes());
            if (Boolean.TRUE.equals(acquire)) {
                return true;
            } else {
                byte[] value = redisConnection.stringCommands().get(lock.getBytes());
                if (Objects.nonNull(value) && value.length > 0) {
                    long expireTime = Long.parseLong(new String(value));
                    if (expireTime < System.currentTimeMillis()) {
                        // 如果锁已经过期
                        byte[] oldValue = redisConnection.stringCommands().getSet(lock.getBytes(), String.valueOf(System.currentTimeMillis() + DISTRIBUTE_LOCK_EXPIRE + 1).getBytes());
                        // 防止死锁
                        Assert.notNull(oldValue, "入参有误，原值不可为空");
                        return Long.parseLong(new String(oldValue)) < System.currentTimeMillis();
                    }
                }
            }
            return false;
        });
    }

//      region Redis分布式锁。基于lua脚本 ----

    private static final Long SUCCESS = 1L;
    //    加锁lua
    private static final String LUA_LOCK_SCRIPT =
            """
                    if redis.call('setNx',KEYS[1],ARGV[1])  then 
                       if redis.call('get',KEYS[1])==ARGV[1] then 
                          return redis.call('expire',KEYS[1],ARGV[2]) 
                       else 
                          return 0 
                       end 
                    end 
                    """;
    private final RedisScript<Long> lockRedisScript = new DefaultRedisScript<>(LUA_LOCK_SCRIPT, Long.class);

    //    解锁lua
    private static final String LUA_UNLOCK_SCRIPT =
            """ 
                    if redis.call('get', KEYS[1]) == ARGV[1] then 
                       return redis.call('del', KEYS[1]) 
                    else 
                       return 0 
                    end 
                    """;
    private final RedisScript<Long> unLockRedisScript = new DefaultRedisScript<>(LUA_UNLOCK_SCRIPT, Long.class);

    /**
     * lua-加锁
     *
     * @param lockKey
     * @param value
     * @param expireTime
     * @date 2021/7/29 1:13 下午
     */
    public boolean doLock(String lockKey, String value, Long expireTime) {
        // 开始时间
        var start = Instant.now();
        // 超时500ms，未成功返回失败
        var timeout = 500L;
        while (!lock(lockKey, value, expireTime)) {
            // 超时返回失败
            if (Duration.between(start, Instant.now()).toMillis() >= timeout)
                return false;

            // 视业务调整sleep时间
            LockSupport.parkNanos(TimeUnit.MILLISECONDS.toNanos(50L));
            log.info("{}:等待获取锁中...", lockKey);
        }
        // 到这里一般都是成功了
        return true;
    }

    /**
     * 获取锁
     *
     * @param lockKey    redis的key
     * @param value      redis的value要求是随机串，防止释放其他请求的锁
     * @param expireTime redis的key 的过期时间  单位（秒) 防止死锁，导致其他请求无法正常执行业务（适当设置长一些，避免业务执行完之前过期）
     * @return
     */
    public boolean lock(String lockKey, String value, Long expireTime) {
        if (ObjectUtil.isNull(expireTime))
//            设置默认过期时间
            expireTime = DISTRIBUTE_LOCK_EXPIRE;
        if (StrUtil.isEmpty(lockKey))
            throw new RuntimeException("分布式锁的key不可为空");
//        添加默认前缀
        lockKey = DISTRIBUTE_LOCK_PREFIX + lockKey;

        Object result = stringRedisTemplate.execute(lockRedisScript, Collections.singletonList(lockKey), value, String.valueOf(expireTime));
        return SUCCESS.equals(result);
    }

    /**
     * 释放锁
     *
     * @param lockKey redis的key
     * @param value   redis的value  只有value比对一致，才能确定是本请求 加的锁 才能正常释放
     * @return
     */
    public boolean unlock(String lockKey, String value) {

        if (StrUtil.isEmpty(lockKey))
            throw new RuntimeException("分布式锁的key不可为空");
//        添加默认前缀
        lockKey = DISTRIBUTE_LOCK_PREFIX + lockKey;

        try {
            Object result = stringRedisTemplate.execute(unLockRedisScript, Collections.singletonList(lockKey), value);
            if (SUCCESS.equals(result)) {
                return true;
            }
        } catch (Exception e) {
            log.error("分布式锁解锁失败。原因：{}，key值：{}, value值：{}", e.getMessage(), lockKey, value);
        }
        return false;
    }

//      endregion ----
}
