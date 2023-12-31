/*
 *    Copyright (c) 2023-2024.  lWoHvYe(Hongyan Wang)
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

package sample.web;

import com.lwohvye.core.utils.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import sample.repo.CustomizeUserRepository;

import java.time.Duration;

@Slf4j
@RestController
public class NumberController {

    @Autowired
    CustomizeUserRepository customizeUserRepository;

    // http://127.0.0.1:8080/res/res-mvc/concatNumbers
    @GetMapping(value = "/api/concatNumbers", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<String> generateConcatNumbers() {
        // 创建包含初始字符串 "starting" 的 Flux
        Flux<String> startingFlux = Flux.just("Starting");

        // 创建包含 1 到 10 的数字的 Flux
        Flux<Integer> numbers = Flux.range(1, 10);

        // 创建包含结束字符串 "end" 的 Flux
        Flux<String> endFlux = Flux.just("End");

        var username = SecurityUtils.getCurrentUsername(); // 这个不能放到下面的fromCallable里，不然拿不到
        var blockingWrapper = Mono.fromCallable(() -> {
            var customizeUser = customizeUserRepository.findByUsername(username);
            return customizeUser.toString();
        });
        var customizeUserMono = blockingWrapper.subscribeOn(Schedulers.boundedElastic());

        // 使用 Flux.concat() 组合多个 Flux，并将每次返回的整数转换为字符串类型，然后返回字符串类型的流。
        return Flux.concat(startingFlux,
                        numbers.map(String::valueOf),
                        customizeUserMono,
                        endFlux)
                .doFirst(() -> log.info("start generate response"))
                .doFinally(signalType -> {
                    switch (signalType) {
                        case ON_COMPLETE, CANCEL -> log.info("Complete Success");
                        case ON_ERROR -> log.info("Terminate by Error");
                        default -> log.info("Terminate by other reason");

                    }
                })
                .delayElements(Duration.ofSeconds(1));
    }
}
