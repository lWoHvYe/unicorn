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

// PeriodicPublisher.java
package com.lwohvye.rx;

import java.util.Random;
import java.util.concurrent.Flow.Subscriber;
import java.util.concurrent.SubmissionPublisher;
import java.util.concurrent.TimeUnit;

// 发布者；创建发布者取决于Flow.Publisher<T>接口的实现类。发布有阻塞的submit和非阻塞的offer
public class PeriodicPublisher {
    final static int MAX_SLEEP_DURATION = 3;
    // Used to generate sleep time
    final static Random sleepTimeGenerator = new Random();

    public static void main(String[] args) {
        SubmissionPublisher<Long> pub = new SubmissionPublisher<>();
        // Create three subscribers
        SimpleSubscriber sub1 = new SimpleSubscriber("S1", 2);
        SimpleSubscriber sub2 = new SimpleSubscriber("S2", 5);
        SimpleSubscriber sub3 = new SimpleSubscriber("S3", 6);
        SimpleSubscriber sub4 = new SimpleSubscriber("S4", 10);
        // Subscriber to the publisher
        pub.subscribe(sub1);
        pub.subscribe(sub2);
        pub.subscribe(sub3);
        // Subscribe the 4th subscriber after 2 seconds
        subscribe(pub, sub4, 2);
        // Start publishing items
        Thread pubThread = publish(pub, 5);
        try {
            // Wait until the publisher is finished
            pubThread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public static Thread publish(SubmissionPublisher<Long> pub, long count) {
        Thread t = new Thread(() -> {
            for (long i = 1; i <= count; i++) {
                // submit 1 to 5
                pub.submit(i);
                sleep(i);
            }
            // Close the publisher
            pub.close();
        });
        // Start the thread
        t.start();
        return t;
    }

    private static void sleep(Long item) {
        // Wait for 1 to 3 seconds
        int sleepTime = sleepTimeGenerator.nextInt(MAX_SLEEP_DURATION) + 1;
        try {
            System.out.printf("Published %d. Sleeping for %d sec.%n", item, sleepTime);
            TimeUnit.SECONDS.sleep(sleepTime);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static void subscribe(SubmissionPublisher<Long> pub, Subscriber<Long> sub, long delaySeconds) {
        new Thread(() -> {
            try {
                TimeUnit.SECONDS.sleep(delaySeconds);
                pub.subscribe(sub);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }).start();
    }
}
