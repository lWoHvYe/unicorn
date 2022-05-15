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

// FilterProcessor.java
package com.lwohvye.rx;

import java.util.concurrent.Flow;
import java.util.concurrent.Flow.Processor;
import java.util.concurrent.SubmissionPublisher;
import java.util.function.Predicate;

// 处理者；处理者（Processor）同时是订阅者也是发布者。 要使用处理者，需要一个实现Flow.Processor<T，R>接口的类，其中T是订阅元素类型，R是已发布的元素类型。
public class FilterProcessor<T> extends SubmissionPublisher<T> implements Processor<T, T> {
    private Predicate<? super T> filter;

    public FilterProcessor(Predicate<? super T> filter) {
        this.filter = filter;
    }

    @Override
    public void onSubscribe(Flow.Subscription subscription) {
        // Request an unbounded number of items
        subscription.request(Long.MAX_VALUE);
    }

    @Override
    public void onNext(T item) {
        // If the item passes the filter publish it. Otherwise, no action is needed.
        System.out.printf("Filter received: %s%n", item);
        if (filter.test(item)) {
            this.submit(item);
        } else {
            System.out.printf(" %s 不满足要求，舍弃 %n", item);
        }
    }

    @Override
    public void onError(Throwable t) {
        // Pass the onError message to all subscribers asynchronously
        this.getExecutor().execute(() -> this.getSubscribers()
                .forEach(s -> s.onError(t)));
    }

    @Override
    public void onComplete() {
        System.out.println("Filter is complete.");
        // Close this publisher, so all its subscribers will receive a onComplete message
        this.close();
    }
}
