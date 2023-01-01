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

package com.lwohvye.reactive.config.security;

import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.ReactiveUserDetailsService;
import org.springframework.util.CollectionUtils;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

@RequiredArgsConstructor
public class SimpleAuthFilter implements WebFilter {

    private final ReactiveUserDetailsService userDetailsService;

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        // TODO: 2022/9/30 待完善。当前SecurityContextHolder中的属性用不了，推测是react下context有问题
        // https://docs.spring.io/spring-security/reference/reactive/index.html
        var request = exchange.getRequest();
        var gwuNames = request.getHeaders().get("GWUName");
        if (!CollectionUtils.isEmpty(gwuNames)) {
            var blockingWrapper = Mono.fromCallable(() -> {
                var username = gwuNames.get(0);
                return userDetailsService.findByUsername(username).block();/* make a remote synchronous call */
            });
            blockingWrapper = blockingWrapper.subscribeOn(Schedulers.boundedElastic());
            blockingWrapper.subscribe(userDetails -> {
                Authentication authentication = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
                SecurityContextHolder.getContext().setAuthentication(authentication);
            });
        }
        // 继续下一个过滤器链的调用
        return chain.filter(exchange);
    }
}
