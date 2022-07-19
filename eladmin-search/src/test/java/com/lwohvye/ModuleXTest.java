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

package com.lwohvye;

import com.lwohvye.command.SimHTTPServiceCommand;
import com.lwohvye.search.modules.handler.SimRestErrorHandler;
import org.junit.jupiter.api.Test;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class ModuleXTest {

    // 模块API https://www.lwohvye.com/2022/03/17/jpms-%e6%a8%a1%e5%9d%97api/
    @Test
    public void moduleInfo() {
        Class<? extends ModuleXTest> aClass = this.getClass();
        var classLoader = aClass.getClassLoader();
        var module = aClass.getModule();
    }


    @Test
    public void httpGetTest() throws ExecutionException, InterruptedException {
        var restTemplate = new RestTemplate();
        restTemplate.setErrorHandler(new SimRestErrorHandler());
        var url = "https://www.lwohvye.com";
        var futures = new ArrayList<Future<String>>();
        for (int i = 0; i < 12; i++) { // 共24个，8个直接执行，12个进队列，4个被直接拒绝
            var postFuture = new SimHTTPServiceCommand(restTemplate, "common", url, Map.of("username", "WHY", "pw", "000000")).queue();
            var getFuture = new SimHTTPServiceCommand(restTemplate, "common", url, null).queue();
            // System.out.println("get " + getFuture);
            // System.out.println("post " + postFuture.get());
            futures.add(postFuture);
            futures.add(getFuture);
        }
        for (Future<String> future : futures) {
            System.out.println("----" + future.get());
        }
    }

    @Test
    public void httpGetTest2() throws ExecutionException, InterruptedException {
        /**
         * 使用 使用restTemplate访问restful接口非常的简单
         * (url, requestMap,ResponseBean.class)这三个参数分别代表 REST请求地址、请求参数、HTTP响应转换被转换成的对象类型。
         */
        var restTemplate = new RestTemplate();
        var url = "https://www.lwohvye.com";
        var postFuture = new SimHTTPServiceCommand(restTemplate, "common", url, Map.of("username", "WHY", "pw", "000000")).queue();
        System.out.println(new SimHTTPServiceCommand(restTemplate, "common", url, null).execute());
        System.out.println(postFuture.get());
    }
}
