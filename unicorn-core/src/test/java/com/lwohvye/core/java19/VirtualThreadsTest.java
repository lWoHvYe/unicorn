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

package com.lwohvye.core.java19;

import jdk.incubator.concurrent.StructuredTaskScope;
import org.junit.jupiter.api.Test;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class VirtualThreadsTest {

    @Test
    void handle() throws ExecutionException, InterruptedException {
        System.out.println(Thread.currentThread().getName() + "    " + Thread.currentThread().isVirtual());
        try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
            Future<String> user = scope.fork(this::findUser);
            Future<Integer> order = scope.fork(this::fetchOrder);

            scope.join();           // Join both forks
            scope.throwIfFailed();  // ... and propagate errors

            // Here, both forks have succeeded, so compose their results
            System.out.println("new Response(user.resultNow(), order.resultNow()) = " + new Response(user.resultNow(), order.resultNow()));
        }
        System.out.println(Thread.currentThread().getThreadGroup().getName() + "    " + Thread.currentThread().isVirtual());
    }

    private Integer fetchOrder() throws InterruptedException {
        System.out.println(Thread.currentThread().getThreadGroup().getName() + "    " + Thread.currentThread().isVirtual());
        Thread.sleep(10000);
        return 19;
    }

    private String findUser() throws InterruptedException {
        System.out.println(Thread.currentThread().getThreadGroup().getName() + "    " + Thread.currentThread().isVirtual());
        Thread.sleep(5000);

        // var virtualFactory = Thread.ofVirtual().factory();
        // // 这个默认就是VirtualTypeFactory，也可以自己传一个
        // try (var tvt = new StructuredTaskScope<String>("TVT", virtualFactory)) {
        //
        // }
        return "user";
    }
}
