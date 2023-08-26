/*
 * Copyright 2002-2023 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.core.task

import java.util.concurrent.ThreadFactory

/**
 * Internal delegate for virtual thread handling on JDK 21.
 * This is the actual version compiled against JDK 21.
 * [...](https://github.com/spring-projects/spring-framework/blob/main/spring-core/src/main/java21/org/springframework/core/task/VirtualThreadDelegate.java)
 *
 * @author Juergen Hoeller
 * @see VirtualThreadTaskExecutor
 *
 * @since 6.1
 */
internal class VirtualThreadDelegate {
    private val threadBuilder: Thread.Builder = Thread.ofVirtual()
    fun virtualThreadFactory(): ThreadFactory {
        return threadBuilder.name("Virtual-Spring").factory()
    }

    fun virtualThreadFactory(threadNamePrefix: String?): ThreadFactory {
        return threadBuilder.name(threadNamePrefix, 0).factory()
    }

    fun newVirtualThread(name: String?, task: Runnable?): Thread {
        return threadBuilder.name(name).unstarted(task)
    }
}
