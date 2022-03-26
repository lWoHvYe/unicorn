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
package com.lwohvye.modules.security.rest;

import cn.hutool.core.util.IdUtil;
import cn.hutool.core.util.RandomUtil;
import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.modules.security.config.bean.LoginCodeEnum;
import com.lwohvye.modules.security.config.bean.LoginProperties;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import com.lwohvye.utils.SecurityUtils;
import com.lwohvye.utils.redis.RedisUtils;
import com.wf.captcha.base.Captcha;
import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * @author Zheng Jie
 * @date 2018-11-23
 * 授权、根据token获取用户详细信息
 */
@Slf4j
@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
@Tag(name = "AuthorizationController", description = "系统：系统授权接口")
public class AuthorizationController {
    private final SecurityProperties properties;
    //    缓存
    private final RedisUtils redisUtils;
    //    Redisson使用
    private final RedissonClient redissonClient;

    @Resource
    private LoginProperties loginProperties;


    @Operation(summary = "获取用户信息")
    @GetMapping(value = "/info")
    public ResponseEntity<Object> getUserInfo() {
        return ResponseEntity.ok(SecurityUtils.getCurrentUser());
    }

    @Operation(summary = "获取验证码")
    @AnonymousGetMapping(value = "/code")
    public ResponseEntity<Object> getCode() {
        // 获取运算的结果
        Captcha captcha = loginProperties.getCaptcha();
        String uuid = properties.getCodeKey() + IdUtil.simpleUUID();
        //当验证码类型为 arithmetic时且长度 >= 2 时，captcha.text()的结果有几率为浮点型
        String captchaValue = captcha.text();
        if (captcha.getCharType() - 1 == LoginCodeEnum.arithmetic.ordinal() && captchaValue.contains(".")) {
            captchaValue = captchaValue.split("\\.")[0];
        }
        // 保存
        redisUtils.set(uuid, captchaValue, loginProperties.getLoginCode().getExpiration(), TimeUnit.MINUTES);
        // 验证码信息
        var imgResult = Map.of("img", captcha.toBase64(), "uuid", uuid);
        return ResponseEntity.ok(imgResult);
    }

    /**
     * Redisson中lock的使用
     *
     * @param request
     * @return org.springframework.http.ResponseEntity
     * @date 2021/10/27 13:35
     */
    @Hidden
    @GetMapping(value = "/doBusiness5Lock")
    public ResponseEntity<Object> doBusiness5Lock(HttpServletRequest request) {

        // --------------Session相关，可考虑接入Redisson的 Spring会话管理 (Spring Session Manager)--------------

        // 获取Session
        var session = request.getSession();
        // SessionId
        var sessionId = session.getId();
        log.info("CurSessionId is : {}", sessionId);
        // 设置属性
        session.setAttribute("sysName", "el-Auth");
        // 获取属性
        var sysName = session.getAttribute("sysName");

        // ---------------------------------锁相关---------------------------------------------

        // region Java 锁 Lock
        var reentrantLock = new ReentrantLock();
        var condition = reentrantLock.newCondition(); // 条件对象。基于Condition，可以更细粒度的控制等待与唤醒
        reentrantLock.lock();
        // var lockRes = reentrantLock.tryLock(); 加锁成功返回true，否则返回false，这样不会阻塞，也可以设置超时
        try {
            while (!RandomUtil.randomBoolean()) // 条件不满足时，保持await()。这样写避免虚假唤醒
                condition.await();
            // doSomething
            condition.signalAll(); // 在其中线程中唤醒。被唤醒的线程要重新获取锁
        } catch (InterruptedException e) {
            log.error(e.getMessage());
            Thread.currentThread().interrupt(); // 异常后，中断线程。这里只是标记中断，具体中断事宜由线程自己处理
        } finally {
            reentrantLock.unlock();
        }

        //---------------------------------------------------------------

        var readWriteLock = new ReentrantReadWriteLock(); // 读写锁
        readWriteLock.readLock().lock(); // 读锁不阻塞读，但阻塞写。Idea快捷键 RL
        try {
            // doSomething
        } finally {
            readWriteLock.readLock().unlock();
        }

        readWriteLock.writeLock().lock(); // 写锁阻塞写和读。Idea快捷键 WL
        try {
            // doSomething
        } finally {
            readWriteLock.writeLock().unlock();
        }

        // endregion

        // 主体感觉上，Java中的锁是单系统的。下面这些Redisson都是 分布式锁。
        // TODO: 2021/12/29 当下遗留一个问题，就是加解锁放在事务中时，解锁后事务还未提交，应如何解决这类问题。
        //  当下的一个方案是将加解锁放到事务外/或者调整其传播行为为独立的事务(Propagation.REQUIRES_NEW)。
        //  从系统设计的角度，会变的复杂一些：若是对资源的使用，需考虑主体业务失败后的补偿问题，也许需要把资源的使用分阶段？（未使用、已锁定、已使用、已失效）
        // region Redisson  可重入锁 RLock
        // 获取分布式锁。只要锁名称一样，就是同一把锁
        // 可重入锁：同一线程不必重新获取锁
        var lock = redissonClient.getLock("lock-red");

        // 枷锁（包含阻塞等待），默认过期时间30s
        // 注：加锁时可指定过期时间，默认30秒，内部有额外的程序，在实例被关闭前，每30秒进行一次续期。所以若出现故障，最多30秒会自动解锁。
        // 若显示指定了过期时间，应该就不会再做续期的逻辑。
        lock.lock();
        try {
            // doSomething...
        } finally {
            // 解锁。
            if (lock.isLocked() && lock.isHeldByCurrentThread()) // 添加这个，目的是保证目标以及锁了，以及比较重要的锁是当前线程锁的
                lock.unlock();
        }

        // Redisson还提供了联锁（MultiLock）和红锁（RedLock），用于关联多个锁对象（可能来自不同的Redisson实例），加解锁保证几个锁都成功获取或释放（联锁保证锁全部加成功，红锁保证在大部分节点锁加成功）
        /*
        // 锁可以来自不同的实例
        var lock1 = redissonClient1.getLock("lock1");
        var lock2 = redissonClient2.getLock("lock2");
        var lock3 = redissonClient3.getLock("lock3");

        var multiLock = new RedissonMultiLock(lock1, lock2, lock3);
        // 同时加锁：lock1 lock2 lock3
        // 所有的锁都上锁成功才算成功。
        multiLock.lock();
        ...
        multiLock.unlock();
        -----------------------------------------------------------------
        var redLock = new RedissonRedLock(lock1, lock2, lock3);
        // 同时加锁：lock1 lock2 lock3
        // 红锁在大部分节点上加锁成功就算成功。
        redLock.lock();
        ...
        redLock.unlock()
        */
        // endregion

        // region   读写锁 RReadWriteLock
        //  读写锁：读读共享、读写互斥、写写互斥
        var rwLock = redissonClient.getReadWriteLock("lock-read_write");
        //  读锁
        var rLock = rwLock.readLock();
        //  写锁
        var wLock = rwLock.writeLock();
        // 加读锁
        rLock.lock();
        //  加写锁
        wLock.lock();
        try {
            // doSomething...
        } finally {
            //  解锁
            rLock.unlock();
            wLock.unlock();
        }
        // endregion

        // region   信号量 RSemaphore
        var semaphore = redissonClient.getSemaphore("semaphore-red");
        //  实际使用时，release() 和 acquire() 在不同的业务/线程中
        //  信号量 +1
        semaphore.release();

        try {
            //  信号量 -1。当信号量为0时，会阻塞
            semaphore.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        // endregion

        // region   闭锁 RCountDownLatch
        var countDownLatch = redissonClient.getCountDownLatch("anyCountDownLatch-green");
        //  等待的量
        countDownLatch.trySetCount(4L);

        //  减少量；这个在实际业务中，会在其他业务/方法里
        countDownLatch.countDown();

        try {
            //  当通过countDown() 减到0时，再执行
            countDownLatch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        // endregion

        // region   限流器RateLimiter
        // 基于Redis的分布式限流器（RateLimiter）可以用来在分布式环境下现在请求方的调用频率。
        // 既适用于不同Redisson实例下的多线程限流，也适用于相同Redisson实例下的多线程限流。
        // 该算法不保证公平性。除了同步接口外，还提供了异步（Async）、反射式（Reactive）和RxJava2标准的接口。
        RRateLimiter rateLimiter = redissonClient.getRateLimiter("authRateLimiter");
        // 初始化
        // 最大流速 = 每1秒钟产生10个令牌。令牌与信号量是不同的，信号量包括获取及释放，而令牌定时产生，只需要取即可
        // 分布式的特点就是，一个实例的动作，可以影响到其他的实例
        rateLimiter.trySetRate(RateType.OVERALL, 10, 1, RateIntervalUnit.SECONDS);

        var rPermits = rateLimiter.availablePermits(); // 可用令牌数
        var rPermitsRFuture = rateLimiter.availablePermitsAsync(); // 异步
        var done = rPermitsRFuture.isDone(); // 异步完成情况
        try {
            var res01 = rPermitsRFuture.get(); // 阻塞获取，可加超时
        } catch (InterruptedException | ExecutionException e) {
            e.printStackTrace();
        }

        // CompletableFuture的getNow(T valueIfAbsent)，本质为Returns the result value (or throws any encountered exception) if completed, else returns the given valueIfAbsent.
        var resNow01 = rPermitsRFuture.getNow(); // 这里应该是要么返回具体结果，要么就是null之类的，不会阻塞
        // 异步编程，主体就是Future，而对Future的操作，主体就是那几种

        // 获取令牌
        rateLimiter.acquire(3); // 阻塞

        var res = rateLimiter.tryAcquire(3);// 非阻塞，还可以带超时

        var resRFuture = rateLimiter.tryAcquireAsync(3); // 异步，返回是RFuture，参照Future相关操作
        // ...

        Thread t = new Thread(() -> {
            // 可以在子线程中，或者其他服务实例中获取令牌
            rateLimiter.acquire(2);
            // ...doSomething()
        });

        // endregion

        // region RKeys
        RMap map = redissonClient.getMap("mymap"); // 支持map的相关操作，还有各种锁，属于本map纬度的各种锁（本质是把map和给定的key拼了一下）
        map.getName(); // = mymap

        RKeys keys = redissonClient.getKeys();

        Iterable<String> allKeys = keys.getKeys();
        Iterable<String> foundedKeys = keys.getKeysByPattern("key*");
        long numOfDeletedKeys = keys.delete("obj1", "obj2", "obj3");
        long deletedKeysAmount = keys.deleteByPattern("test?");
        String randomKey = keys.randomKey();
        long keysAmount = keys.count();
        // endregion

        // region Object Bucket 通用对象桶
        RBucket<String> bucket = redissonClient.getBucket("anyObject");
        bucket.set("str1");
        var obj = bucket.get();

        bucket.trySet("str3");
        bucket.compareAndSet("str4", "str5"); // CAS
        bucket.getAndSet("str6");
        // endregion

        // region HyperLogLog 基数估计算法
        RHyperLogLog<Integer> hpLog = redissonClient.getHyperLogLog("hpLog");
        hpLog.add(1);
        hpLog.add(2);
        hpLog.add(3);

        hpLog.count(); // 里面方法也挺多的

        var longRFuture = hpLog.countAsync();
        if (longRFuture.isDone()) {
            try {
                var aLong = longRFuture.get();
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
            }
        }
        // endregion

        // region LongAdder 整长型累加器。对于需要并发对同一对象加减的操作，累加器比Atomic系列性能要高很多
        RLongAdder atomicLong = redissonClient.getLongAdder("myLongAdder");

        atomicLong.add(12);
        atomicLong.increment();
        atomicLong.decrement();
        atomicLong.sum();

        // 当不再使用整长型累加器对象的时候应该自行手动销毁，如果Redisson对象被关闭（shutdown）了，则不用手动销毁。
        atomicLong.destroy();

        // endregion

        return null;
    }
}
