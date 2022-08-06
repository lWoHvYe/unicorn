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

package com.lwohvye.thread;

import com.lwohvye.utils.result.ResultInfo;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.concurrent.*;

public class ExecutorCompletionServiceSample {

    @Test
    public void rpcWork() {
        var executorService = Executors.newCachedThreadPool();
        // 当单次逻辑中提交多个异步任务，需要根据执行完成的顺序而非提交的顺序，获取结果时，可使用ExecutorCompletionService
        var completionService = new ExecutorCompletionService<ResultInfo>(executorService);
        var futures = new ArrayList<Future<ResultInfo>>();

        futures.add(completionService.submit(() -> {
            var name = Thread.currentThread().getName();
            System.out.println("休息30秒");
            TimeUnit.SECONDS.sleep(30);
            System.out.println("休息结束");
            return ResultInfo.success(name);
        }));
        futures.add(completionService.submit(() -> {
            var name = Thread.currentThread().getName();
            System.out.println("休息20秒");
            TimeUnit.SECONDS.sleep(20);
            System.out.println("休息结束");
            return ResultInfo.validateFailed(name);
        }));
        futures.add(completionService.submit(() -> {
            var name = Thread.currentThread().getName();
            System.out.println("休息15秒");
            TimeUnit.SECONDS.sleep(15);
            System.out.println("休息结束");
            return ResultInfo.success(name);
        }));
        futures.add(completionService.submit(() -> {
            var name = Thread.currentThread().getName();
            System.out.println("休息60秒");
            TimeUnit.SECONDS.sleep(60);
            System.out.println("休息结束");
            return ResultInfo.success(name);
        }));

        for (int i = 0; i < 4; i++) {
            try {
                // 注意：只有调用了 ExecutorCompletionService 的take或poll时，阻塞队列中的 task 执行结果才会从队列中移除掉，释放堆内存，无论是否需要返回结果，都不要忘记调用释放内存
                var result = completionService.take().get();
                if (result.getBusinessCode() == 200) {
                    // 成功时输出结果
                    System.out.println(result.getResult());
                } else {
                    System.out.printf("%s 执行失败，任务取消%n", result.getDescription());
                    // 存着非成功时，取消所有的任务，并退出
                    for (var future : futures) {
                        future.cancel(true);
                    }
                    break;
                }
            } catch (InterruptedException | ExecutionException ignored) {
            }
        }
    }
}
