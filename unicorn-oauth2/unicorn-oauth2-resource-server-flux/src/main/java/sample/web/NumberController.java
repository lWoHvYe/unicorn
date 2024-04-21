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

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import sample.domain.CustomizeUser;
import sample.dto.UserVO;
import sample.repo.CustomizeUserRepository;
import sample.utils.ReactiveSecurityUtils;

import java.time.Duration;

@Slf4j
@RestController
public class NumberController {

    @Autowired
    CustomizeUserRepository customizeUserRepository;

    // 在使用 Server-Sent Events (SSE) 时，WebFlux 会将每个返回的数据项包装在 "data:" 字段中，以便客户端能够正确解析数据。这是为了遵循 SSE 规范，以便客户端可以根据 "data:" 字段识别并处理每个数据项。
    //
    //  SSE 是一种用于实现服务器到客户端的单向数据传输的通信协议。在 SSE 中，服务器可以向客户端发送一系列数据项，而客户端通过监听连接来接收这些数据项。
    //  每个数据项通常表示一个事件，服务器将数据项按照一定的格式发送给客户端，以便客户端可以正确处理它们。
    //
    //  在 SSE 中，每个数据项被包含在一个称为事件流（event stream）的数据流中。而每个数据项都以 "data:" 字段开头，然后是实际的数据内容。这样客户端就能够识别数据项，并根据 "data:" 字段来解析数据。
    // 使用 produces = MediaType.TEXT_EVENT_STREAM_VALUE 来指定响应的数据类型为 Server-Sent Events (SSE)，这样客户端可以以流的方式接收数据。
    @GetMapping(value = "/api/numbers", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<Integer> generateNumbers() {
        // 创建一个包含 1 到 10 的数字的 Flux
        Flux<Integer> numbers = Flux.range(1, 10);

        // 每秒返回一个数字
        return numbers.delayElements(Duration.ofSeconds(1));
    }

    // http://127.0.0.1:8080/res/res-flux/api/concatNumbers
    @GetMapping(value = "/api/concatNumbers", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<String> generateConcatNumbers() {
        // 创建包含初始字符串 "starting" 的 Flux
        Flux<String> startingFlux = Flux.just("Starting");

        // 创建包含 1 到 10 的数字的 Flux
        Flux<Integer> numbers = Flux.range(1, 10);

        // 创建包含结束字符串 "end" 的 Flux
        Flux<String> endFlux = Flux.just("End");

        // 使用 Flux.concat() 组合多个 Flux，并将每次返回的整数转换为字符串类型，然后返回字符串类型的流。
        return Flux.concat(startingFlux,
                        customizeUserRepository.findByUsername(ReactiveSecurityUtils.getCurrentUsername())
                                .map(CustomizeUser::toString),
                        numbers.map(String::valueOf),
                        customizeUserRepository.findByUsername(ReactiveSecurityUtils.getCurrentUsernameSham())
                                .map(CustomizeUser::toString),
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

    // 这个会触发file download
    // http://127.0.0.1:8080/res/res-flux/api/concatObj
    @GetMapping(value = "/api/concatObj", produces = MediaType.APPLICATION_NDJSON_VALUE) // 如果返回一个对象，用这个返回json格式
    public Flux<CustomizeUser> generateConcatObj() {

        // 使用 Flux.concat() 组合多个 Flux
        return Flux.concat(
                        customizeUserRepository.findByUsername(ReactiveSecurityUtils.getCurrentUsername()),
                        customizeUserRepository.findByUsername(ReactiveSecurityUtils.getCurrentUsernameSham())
                )
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

    // 如果返回一个对象，用这个返回json格式，同时用consumes指明入参格式
    @PostMapping(value = "/api/combineObj", consumes = MediaType.APPLICATION_NDJSON_VALUE, produces = MediaType.APPLICATION_NDJSON_VALUE)
    public Flux<CustomizeUser> combineObj(@RequestBody Flux<UserVO> userNames) {
        // 使用 Flux.concat() 组合多个 Flux
        return Flux.concat(customizeUserRepository.findByUsername(ReactiveSecurityUtils.getCurrentUsername()),
                        userNames.flatMap(user -> customizeUserRepository.findByUsername(user.getUsername())),
                        customizeUserRepository.findByUsername(ReactiveSecurityUtils.getCurrentUsernameSham()))
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
