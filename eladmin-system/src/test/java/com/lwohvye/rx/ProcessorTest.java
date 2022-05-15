/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

// ProcessorTest.java
package com.lwohvye.rx;

import java.util.concurrent.SubmissionPublisher;
import java.util.concurrent.TimeUnit;
import java.util.stream.LongStream;

public class ProcessorTest {
    /**
     * 处理者的基础测试
     *
     * @param args
     * @date 2022/5/15 4:32 PM
     */
    public static void main(String[] args) {
        // CompletableFuture<Void> subTask = null;
        // The publisher is closed when the try block exits
        try (var pub = new SubmissionPublisher<Long>()) {
            // Create a Subscriber
            var sub = new SimpleSubscriber("S1", 5);
            // Create a processor
            var filter = new FilterProcessor<Long>(n -> n % 2 == 0);
            // Subscribe the filter to the publisher and a subscriber to the filter
            pub.subscribe(filter);
            filter.subscribe(sub);
            // Generate and publish 14 integers
            LongStream.range(1L, 15L)
                    .forEach(pub::submit);
        }
        try {
            // Sleep for two seconds to let subscribers finish handling all items
            TimeUnit.SECONDS.sleep(2);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
