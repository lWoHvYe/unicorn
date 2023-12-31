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

package sample.utils;

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
    // 这段代码是一个用于获取当前用户名的方法，返回一个 `Mono<String>`。它使用了 Reactor 的响应式编程风格。
    //总体而言，这段代码使用了 Reactor 提供的响应式操作符对安全上下文进行处理，并返回一个 `Mono`，以支持异步非阻塞的响应式编程模型。
    public static Mono<String> getCurrentUsername() {
        //1. 首先，通过 `ReactiveSecurityContextHolder.getContext()` 获取当前的安全上下文。
        return ReactiveSecurityContextHolder.getContext()
                //2. 使用 `filter` 过滤掉没有进行身份验证的安全上下文。
                .filter(c -> c.getAuthentication() != null)
                //3. 使用 `map` 将安全上下文转换为认证信息 `Authentication`。
                .map(SecurityContext::getAuthentication)
                //4. 使用 `as` 将 `Mono<Authentication>` 转换为 `Mono<String>`。
                .as(authenticationMono ->
                        //5. 在 `authenticationMono` 上使用 `handle` 处理操作，它接收一个 `BiConsumer`，可以根据 `authentication` 的类型进行处理。
                        //
                        // 这段代码使用了 Java 8 中的新特性之一，称为目标类型推断（Target Type Inference）。
                        //在这里，`authenticationMono` 是一个泛型类型为 `Mono<Authentication>` 的变量。通过使用目标类型推断，可以省略 `handle` 方法的泛型类型参数，因为编译器可以根据上下文推断出泛型类型为 `String`。
                        //在旧版本的 Java 中，需要显式指定 `handle` 方法的泛型类型参数，如 `authenticationMono.handle((Authentication authentication, SynchronousSink<String> sink) -> { ... })`。
                        // 但是在 Java 8 之后，引入了目标类型推断，允许编译器根据上下文推断泛型类型，从而简化代码。
                        //因此，`authenticationMono.<String>handle(...)` 可以简化为 `authenticationMono.handle(...)`，让代码更加简洁和易读。但在这里，不能进行简化掉<String>
                        //
                        // 在 Reactor 3.4.0 版本中，添加了 `.handle(BiFunction)` 方法的重载，其中可以指定结果类型的泛型参数。这允许您在操作链中的 `.handle` 方法中明确指定结果的类型，以提供更好的类型安全和编译时检查。
                        //具体到您提到的代码片段 `authenticationMono.<String>handle(...)`，在这里 `<String>` 是在 `.handle` 方法之前的结构，用于指定结果类型为 `String`。
                        //在较早的版本中（如 Reactor 3.3.x 及更早版本），`.handle` 方法没有泛型参数，返回的是 `Mono`。这意味着您需要在操作链中使用额外的操作符或显式转换来指定结果的类型。
                        //要注意的是，Reactor 版本的功能和语法可能会有所变化。因此，确保使用的是适用版本的 Reactor 和相关库，并参考它们的文档以了解具体的版本变化和支持的特性。
                        authenticationMono.<String>handle((authentication, sink) -> {
                            var principal = authentication.getPrincipal();
                            //   - 如果 `principal` 是 `UserDetails` 的实例，将其转换为 `UserDetails`，并使用 `sink.next()` 发出用户名。
                            if (principal instanceof UserDetails userDetails) {
                                log.warn("UserDetails.... {} ", userDetails.getUsername());
                                sink.next(userDetails.getUsername());
                                return;
                            }
                            if (principal instanceof Jwt jwt) {
                                var clientDetail = jwt.getSubject() + " -> " + jwt.getIssuer();
                                log.warn("Jwt.... {} n_n {} ", clientDetail, jwt.getClaims().getOrDefault("userId", "ANONYMOUS"));
                                sink.next(jwt.getSubject());
                                return;
                            }
                            //   - 如果 `principal` 不是 `UserDetails` 的实例，使用 `sink.error()` 抛出一个自定义的 `AuthException` 异常。
                            sink.error(new UsernameNotFoundException("找不到当前登录的信息: " + principal));
                        }))
                //6. 使用 `doOnSuccess` 在成功时执行一些操作，这里是记录用户名的日志。
                .doOnSuccess(it -> log.info("userName {} ", it))
                //7. 使用 `doOnError` 在发生错误时执行一些操作，这里是记录错误信息的日志。
                .doOnError(ex -> log.info("error {} ", ex.getMessage()))
                //8. 使用 `defaultIfEmpty` 指定一个默认值，在没有找到用户名时返回 "anonymous"。
                .defaultIfEmpty("anonymous");
    }

    // 在这段代码中，可以考虑进行一些优化和改进，具体如下：
    //1. 减少嵌套：可以使用 `flatMap` 替代 `as`，并将后续的操作链移到 `flatMap` 内部，以减少嵌套层级和提高代码可读性。
    //2. 使用 `switchIfEmpty` 替代 `defaultIfEmpty`：`switchIfEmpty` 操作符可以更直观地指定当流为空时的替代逻辑。
    //3. 使用 `map` 替代 `handle`：由于处理操作只是根据 `principal` 的类型进行转换，可以使用 `map` 操作符结合条件判断来达到相同的效果，而不必使用 `handle`。
    //通过这些优化，代码更加简洁、清晰，并且减少了嵌套层级，提高了代码的可读性和可维护性。
    // map 操作符：
    //  map 操作符用于将每个流中的元素应用一个函数，并将函数的结果包装成一个新的元素，最终返回一个新的流。
    //  map 操作符的函数返回的是一个普通的值，而不是一个包含多个元素的流。
    //  map 操作符的输出流的元素个数和输入流的元素个数相同，一一对应。
    // flatMap 操作符：
    //  flatMap 操作符用于将每个流中的元素应用一个函数，这个函数的返回值是一个新的流，然后将这些新的流合并成一个输出流。
    //  flatMap 操作符的函数返回的是一个包含多个元素的流，因此可以将多个流合并成一个。
    //  flatMap 操作符通常用于处理嵌套的流或执行异步操作。
    //map 用于简单的一对一元素转换。flatMap 用于处理一对多的元素转换或进行流的合并操作。在下例中若将flatMap改为map需return String而非Mono<String>
    public static Mono<String> getCurrentUsernameSham() {
        return ReactiveSecurityContextHolder.getContext()
                .filter(c -> c.getAuthentication() != null)
                .map(SecurityContext::getAuthentication)
                .flatMap(authentication -> {
                    var principal = authentication.getPrincipal();
                    if (principal instanceof UserDetails userDetails) {
                        log.warn("UserDetails.... {} ", userDetails.getUsername());
                        return Mono.just(userDetails.getUsername());
                    }
                    if (principal instanceof Jwt jwt) {
                        var clientDetail = jwt.getSubject() + " -> " + jwt.getIssuer();
                        log.warn("Jwt.... {} n_n {} ", clientDetail, jwt.getClaims().getOrDefault("userId", "ANONYMOUS"));
                        return Mono.just(jwt.getSubject());
                    }
                    return Mono.error(new UsernameNotFoundException("找不到当前登录的信息: " + principal));
                })
                .doOnSuccess(username -> log.info("userName {} ", username))
                .doOnError(ex -> log.info("error {} ", ex.getMessage()))
                .switchIfEmpty(Mono.just("anonymous"));
    }

}
