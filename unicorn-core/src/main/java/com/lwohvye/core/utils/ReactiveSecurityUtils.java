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

package com.lwohvye.core.utils;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.context.ReactiveSecurityContextHolder;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.oauth2.jwt.Jwt;
import reactor.core.publisher.Mono;

@Slf4j
@UtilityClass
public class ReactiveSecurityUtils {

    public static Mono<String> getCurrentUsername() {
        return ReactiveSecurityContextHolder.getContext()
                .filter(c -> c.getAuthentication() != null)
                .map(SecurityContext::getAuthentication)
                .flatMap(authentication -> {
                    var principal = authentication.getPrincipal();
                    if (principal instanceof UserDetails userDetails)
                        return Mono.just(userDetails.getUsername());
                    if (principal instanceof Jwt jwt)
                        return Mono.just(jwt.getSubject());

                    return Mono.error(new UsernameNotFoundException("找不到当前登录的信息: " + principal));
                })
                .doOnSuccess(username -> log.debug("userName {} ", username))
                .doOnError(ex -> log.error("error {} ", ex.getMessage()))
                .switchIfEmpty(Mono.just("anonymous"));
    }

}
