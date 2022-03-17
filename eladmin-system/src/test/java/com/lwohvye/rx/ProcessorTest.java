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

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.SubmissionPublisher;
import java.util.concurrent.TimeUnit;
import java.util.stream.LongStream;

public class ProcessorTest {
    public static void main(String[] args) {
        CompletableFuture<Void> subTask = null;
        // The publisher is closed when the try block exits
        try (SubmissionPublisher<Long> pub = new SubmissionPublisher<>()) {
            // Create a Subscriber
            SimpleSubscriber sub = new SimpleSubscriber("S1", 10);
            // Create a processor
            FilterProcessor<Long> filter = new FilterProcessor<>(n -> n % 2 == 0);
            // Subscribe the filter to the publisher and a subscriber to the filter
            pub.subscribe(filter);
            filter.subscribe(sub);
            // Generate and publish 6 integers
            LongStream.range(1L, 7L)
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
