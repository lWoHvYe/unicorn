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

package sample.repo;

import org.reactivestreams.Publisher;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;
import sample.domain.CustomizeUser;

@Repository
public interface CustomizeUserRepository extends ReactiveCrudRepository<CustomizeUser, Long> {
    Mono<CustomizeUser> findByUsername(Publisher<String> username);

    Mono<CustomizeUser> findByUsername(String username);
    // while there are multi-satisfied result, will get this error
    // reactor.core.Exceptions$ErrorCallbackNotImplemented: java.lang.IllegalArgumentException: Cannot encode class java.util.ArrayList
//    Flux<CustomizeUser> findByUsername(Publisher<String> username);
}
