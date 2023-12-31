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

import reactor.core.publisher.Flux;

import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.LockSupport;

public class NumberGenerator {

    // 使用 Flux.range(1, 10) 创建一个包含 1 到 10 的数字的 Flux。
    // 然后，我们使用 delayElements(Duration.ofSeconds(1)) 操作符来实现每秒返回一个数字的效果。
    // 最后，我们使用 subscribe() 方法来订阅输出，将每秒返回的数字打印到控制台。
    public static void main(String[] args) {
        // 创建一个包含 1 到 10 的数字的 Flux
        Flux<Integer> numbers = Flux.range(1, 10);

        // 每秒返回一个数字，并订阅输出
        numbers.delayElements(Duration.ofSeconds(1))
                .subscribe(System.out::println);

        // 等待一段时间，确保输出完整
        LockSupport.parkNanos(TimeUnit.SECONDS.toNanos(12L));
    }
}
