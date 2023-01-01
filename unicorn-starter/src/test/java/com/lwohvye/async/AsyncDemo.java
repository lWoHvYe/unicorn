/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.async;

import cn.hutool.core.util.RandomUtil;
import org.junit.jupiter.api.Test;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class AsyncDemo {

    // 注意，使用Spring @Async，需要开启注解，这里直接调用是不行的（没有异步），后续再调整一下

    @Test
    public void testAsync() throws ExecutionException, InterruptedException {

        asyncVoid(); // 无返回值的异步方法

        var stringFuture = asyncRes(); // 有返回值的异步方法

        var futureStr = CompletableFuture.supplyAsync(() -> {  // 使用CompletableFuture，可以进行多种组合，可以再看看相关资料
            System.out.println("cp - 开始");
            try {
                if (RandomUtil.randomBoolean())
                    throw new RuntimeException("来个异常");
                Thread.sleep(4500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            System.out.println("cp - 结束");
            return "cp 返回结果";
        });

        while (!stringFuture.isDone()) { // 可以这样判断下是否有做完
            System.out.println("等待返回结果");
        }
        System.out.println("res 结果: " + stringFuture.get());

        futureStr.thenAcceptAsync(System.out::println);

        while (!futureStr.isDone()) {
            System.out.println("等待 cp 完成");
            Thread.sleep(500);
        }
        if (futureStr.isCompletedExceptionally())
            System.out.println("cp 异常结束");
        else System.out.println("cp 正常结束");
        System.out.println("cp 结果：" + futureStr.get()); // 出异常时，get会把异常抛出来
    }

    /**
     * 无返回值的异步
     *
     * @date 2022/3/4 10:33 PM
     */
    // 若异步线程池也需要做隔离，方法为，先通过@Bean定义各线程池，然后@Async("线程池Bean名称")这样来指定异步使用的线程池
    @Async
    public void asyncVoid() {
        System.out.println("void - 开始");
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("void - 结束");
    }

    /**
     * 有返回值的异步
     *
     * @return java.util.concurrent.Future<java.lang.String>
     * @date 2022/3/4 10:40 PM
     */
    @Async
    public Future<String> asyncRes() {
        System.out.println("res - 开始");
        try {
            Thread.sleep(4000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("res - 结束");
        return new AsyncResult<>("res 返回结果"); // 可通过AsyncResult返回结果，返回是Future，应该别的实现也可以，只是用途不同
    }
}
