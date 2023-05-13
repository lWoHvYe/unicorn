/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sample.web;

import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * @author Joe Grandja
 * @since 0.0.1
 */
@RestController
public class SamplesResourceController {

    @GetMapping("/messages")
    public Flux<String> getMessages(@AuthenticationPrincipal Jwt jwt) {
        return Flux.just("Resources-Server-8090", jwt.getSubject(), "Message-1", "Message-2", "Message-3");
    }

    @GetMapping("/resource")
    public Mono<String> retrieveResource(@AuthenticationPrincipal Jwt jwt) {
        return Mono.just(String.format("Resource accessed by %s (sub)", jwt.getSubject()));
    }

    @PostMapping("/resource")
    public Mono<String> createResource(@AuthenticationPrincipal Jwt jwt) {
        return Mono.just(String.format("Resource accessed by %s (sub) create successful", jwt.getSubject()));
    }
}
