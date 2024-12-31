/*
 *    Copyright (c) 2024-2025.  lWoHvYe(Hongyan Wang)
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

package sample.consumer.client;

import org.springframework.http.MediaType;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;
import reactor.core.publisher.Flux;
import sample.consumer.domian.CustomizeUser;

import java.util.Map;

@HttpExchange("/api")
public interface NumberClient {

    @GetExchange(value = "/numbers")
    Flux<Integer> generateNumbers();

    @GetExchange(value = "/concatNumbers")
    Flux<String> generateConcatNumbers();

    @GetExchange(value = "/concatObj")
    Flux<CustomizeUser> generateConcatObj();

    @PostExchange(value = "/combineObj", accept = MediaType.APPLICATION_NDJSON_VALUE, contentType = MediaType.APPLICATION_NDJSON_VALUE)
    Flux<CustomizeUser> combineObj(@RequestBody Flux<UserDetails> userNames, @CookieValue Map<String, Object> cookies);
}
