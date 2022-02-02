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
import org.redisson.api.RedissonClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;
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

        // ---------------------------------Session相关---------------------------------------

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

        // region Java 锁
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
        // region Redisson  可重入锁
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
        // endregion

        // region   读写锁
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

        // region   信号量
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

        // region   闭锁
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

        return null;
    }
}
