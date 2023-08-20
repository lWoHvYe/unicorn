/*
 *    Copyright (c) 2023.  lWoHvYe(Hongyan Wang)
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

package org.springframework.core.task;

import java.util.concurrent.ThreadFactory;

/**
 * Internal delegate for virtual thread handling on JDK 21.
 * This is a dummy version for reachability on JDK <21.
 *
 * @author Juergen Hoeller, lWoHvYe
 * @see VirtualThreadTaskExecutor
 * @since 6.1
 */
final class VirtualThreadDelegate {

    public VirtualThreadDelegate() {
    }

    public ThreadFactory virtualThreadFactory() {
        return Thread.ofVirtual().name("Virtual-Delegate").factory();
    }

    public ThreadFactory virtualThreadFactory(String threadNamePrefix) {
        return Thread.ofVirtual().name(threadNamePrefix, 0).factory();
    }

    public Thread newVirtualThread(String name, Runnable task) {
        return Thread.ofVirtual().name(name).unstarted(task);
    }
}
