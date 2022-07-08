/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.lwohvye.command;

import com.netflix.hystrix.*;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

import java.util.Map;
import java.util.Objects;


public class SimHTTPServiceCommand extends HystrixCommand<String> {
    /*
    hystrix会以执行Command的方式来执行服务的调用，执行Command的方式一共四种

    execute()：以同步堵塞方式执行run()。调用execute()后，hystrix先创建一个新线程运行run()，接着调用程序要在execute()调用处一直堵塞着，直到run()运行完成。

    queue()：以异步非堵塞方式执行run()。调用queue()就直接返回一个Future对象，同时hystrix创建一个新线程运行run()，调用程序通过Future.get()拿到run()的返回结果，而Future.get()是堵塞执行的。

    observe()：事件注册前执行run()/construct()。响应式异步执行，立即获取
       第一步是事件注册前，先调用observe()自动触发执行run()/construct()（如果继承的是HystrixCommand，hystrix将创建新线程非堵塞执行run()；如果继承的是HystrixObservableCommand，将以调用程序线程堵塞执行construct()），
       第二步是从observe()返回后调用程序调用subscribe()完成事件注册，如果run()/construct()执行成功则触发onNext()和onCompleted()，如果执行异常则触发onError()。

    toObservable()：事件注册后执行run()/construct()。响应式异步执行，惰性获取
       第一步是事件注册前，调用toObservable()就直接返回一个Observable<String>对象，
       第二步调用subscribe()完成事件注册后自动触发执行run()/construct()（如果继承的是HystrixCommand，hystrix将创建新线程非堵塞执行run()，调用程序不必等待run()；
          如果继承的是HystrixObservableCommand，将以调用程序线程堵塞执行construct()，调用程序等待construct()执行完才能继续往下走），如果run()/construct()执行成功则触发onNext()和onCompleted()，如果执行异常则触发onError()
    注意：execute()和queue()是在HystrixCommand中，observe()和toObservable()是在HystrixObservableCommand 中。
    从底层实现来讲，HystrixCommand其实也是利用Observable实现的（看Hystrix源码，可以发现里面大量使用了RxJava），尽管它只返回单个结果。
    HystrixCommand的queue方法实际上是调用了toObservable().toBlocking().toFuture()，而execute方法实际上是调用了queue().get()。
     */

    private final RestTemplate restTemplate;
    private final String url;
    private final Map<String, Object> params;

    public SimHTTPServiceCommand(RestTemplate restTemplate, String businessType, String url, Map<String, Object> params) {
        super(initSetter(businessType));
        this.restTemplate = restTemplate;
        this.url = url;
        this.params = params;
    }

    private static Setter initSetter(String businessType) {
        // 服务分组，默认一个服务名一个组
        var groupKey = HystrixCommandGroupKey.Factory.asKey("simHTTP");
        // 服务标识，默认当前执行的方法名称
        var commandKey = HystrixCommandKey.Factory.asKey(businessType);
        // 线程池名称，相同线程池名称的线程池是同一个
        var threadPoolKey = HystrixThreadPoolKey.Factory.asKey(businessType + "-pool");
        // 线程池配置
        var threadPoolProperties = HystrixThreadPoolProperties.Setter()
                // 最大并发执行数，默认10
                .withCoreSize(8)
                // 存活时间（控制一个线程从实用完成到被释放的时间），默认1min
                .withKeepAliveTimeMinutes(5)
                // BlockingQueue的最大长度，正数：队列将从同步队列改为阻塞队列，默认-1
                .withMaxQueueSize(24)
                // 拒绝请求的临界值，默认5，这个与 withMaxQueueSize 配合使用，等待队列的大小，取得是这两个参数的较小值
                .withQueueSizeRejectionThreshold(12); // 如果只设置了线程池大小，另外两个 queue 相关参数没有设置的话，等待队列是处于关闭的状态
        // 命令属性配置
        var commandProperties = HystrixCommandProperties.Setter()
                .withCircuitBreakerEnabled(true)
                .withCircuitBreakerRequestVolumeThreshold(20)
                .withCircuitBreakerErrorThresholdPercentage(40)
                .withCircuitBreakerSleepWindowInMilliseconds(3000)
                // 设置超时时间
                .withExecutionTimeoutInMilliseconds(20000)
                // 设置fallback最大请求并发数
                .withFallbackIsolationSemaphoreMaxConcurrentRequests(30)
                // 隔离策略：THREAD(线程池_默认)、SEMAPHORE(信号量)
                .withExecutionIsolationStrategy(HystrixCommandProperties.ExecutionIsolationStrategy.THREAD);
        return Setter
                .withGroupKey(groupKey)
                .andCommandKey(commandKey)
                .andThreadPoolKey(threadPoolKey)
                .andThreadPoolPropertiesDefaults(threadPoolProperties)
                .andCommandPropertiesDefaults(commandProperties);
    }

    // 执行任务
    @Override
    protected String run() {
        if (Objects.isNull(params))
            return restTemplate.getForObject(url, String.class);
        var httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        var httpEntity = new HttpEntity<>(params, httpHeaders);
        return restTemplate.postForObject(url, httpEntity, String.class);
    }

    // fallback
    @Override
    protected String getFallback() {
        return "调用失败、请稍后重试";
    }

    // Hystrix-RequestCache
    @Override
    protected String getCacheKey() {
        return super.getCacheKey();
    }

    // 合并请求 HystrixCollapser。需要被调用端也支持batch才行
}
