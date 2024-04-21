/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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

import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.security.core.userdetails.User;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;

@RestController
@RequestMapping("/test")
public class TestController {

    @GetMapping(value = "/testCombineObj", produces = MediaType.APPLICATION_NDJSON_VALUE)
    public Flux<String> testCombineObj(ServerHttpRequest request) {
        var users = Flux.just("performer", "unicorn", "test")
                .map(username -> User.withUsername(username).password("why")
                        .build());
        var webClient = WebClient.create("http://127.0.0.1:8080/res/res-flux");

        var cookies = request.getCookies();
        var jsessionid = cookies.get("JSESSIONID").getFirst();
        return webClient
                .post().uri("/api/combineObj")
                .contentType(MediaType.APPLICATION_NDJSON)
                .body(users, User.class)
                .cookie(jsessionid.getName(), jsessionid.getValue())
                .retrieve()
                .bodyToFlux(String.class);
    }
}
