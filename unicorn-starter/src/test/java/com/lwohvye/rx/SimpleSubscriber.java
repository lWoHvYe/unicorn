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

// SimpleSubscriber.java
package com.lwohvye.rx;

import java.util.concurrent.Flow;

// 订阅者；要有订阅者，需要创建一个实现Flow.Subscriber<T>接口的类。 实现接口方法的方式取决于具体的需求。
public class SimpleSubscriber implements Flow.Subscriber<Long> {
    private Flow.Subscription subscription;
    // Subscriber name
    private String name = "Unknown";
    // Maximum number of items to be processed by this subscriber
    private final long maxCount;
    // keep track of number of items processed
    private long counter;

    public SimpleSubscriber(String name, long maxCount) {
        this.name = name;
        this.maxCount = maxCount <= 0 ? 1 : maxCount;
    }

    public String getName() {
        return name;
    }

    @Override
    public void onSubscribe(Flow.Subscription subscription) {
        this.subscription = subscription;
        System.out.printf("%s subscribed with max count %d.%n", name, maxCount);
        // Request all items in one go
        subscription.request(maxCount);
    }

    @Override
    public void onNext(Long item) {
        counter++;
        System.out.printf("%s received %d.%n", name, item);
        if (counter >= maxCount) {
            System.out.printf("Cancelling %s. Processed item count: %d.%n", name, counter);
            // Cancel the subscription
            subscription.cancel();
        }
    }

    @Override
    public void onError(Throwable t) {
        System.out.printf("An error occurred in %s: %s.%n", name, t.getMessage());
    }

    @Override
    public void onComplete() {
        System.out.printf("%s is complete. The count of process is %s %n", name, counter);
    }
}
