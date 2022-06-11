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

package com.lwohvye.starter.modules.rest;

import com.lwohvye.starter.modules.service.ITSService;
import com.lwohvye.starter.modules.service.SimHTTPServiceCommand;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

@RestController
@RequestMapping("/api/anonymous/tsScope")
@RequiredArgsConstructor
public class TSController {
    @Autowired
    private ITSService ITSService;

    /**
     * 使用 使用restTemplate访问restful接口非常的简单
     * (url, requestMap,ResponseBean.class)这三个参数分别代表 REST请求地址、请求参数、HTTP响应转换被转换成的对象类型。
     */
    private final RestTemplate restTemplate = new RestTemplate();

    private String name;


    @GetMapping(value = "/{username}")
    public void userProfile(@PathVariable("username") String username) {
        name = username;

        ITSService.setField(name);

        try {
            for (int i = 0; i < 10; i++) {
                System.out.println(
                        Thread.currentThread().getId()
                        + "name:" + name
                        + "--ts:"
                        + ITSService.outIn());
                Thread.sleep(2000);
            }
        } catch (InterruptedException ignored) {
        }
    }

    @GetMapping(value = "tg")
    public void httpGetTest() throws ExecutionException, InterruptedException {
        var url = "https://www.lwohvye.com";
        var postFuture = new SimHTTPServiceCommand(restTemplate, "common", url, Map.of("username", "WHY", "pw", "000000")).queue();
        System.out.println(new SimHTTPServiceCommand(restTemplate, "common", url, null).execute());
        System.out.println(postFuture.get());
    }
}
