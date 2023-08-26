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
package org.springframework.boot.autoconfigure.thread

import org.springframework.boot.system.JavaVersion
import org.springframework.core.env.Environment

/**
 * Threading of the application.
 *
 * @author Moritz Halbritter, lWoHvYe
 * @since 3.2.0
 */
enum class Threading {
    /**
     * Platform threads. Active if virtual threads are not active.
     */
    PLATFORM {
        override fun isActive(environment: Environment): Boolean {
            return !VIRTUAL.isActive(environment)
        }
    },

    /**
     * Virtual threads. Active if `spring.threads.virtual.enabled` is `true`
     * and running on Java 20 or later.
     */
    VIRTUAL {
        override fun isActive(environment: Environment): Boolean {
            return (environment.getProperty("spring.threads.virtual.enabled", Boolean::class.javaPrimitiveType!!, false)
                    && JavaVersion.getJavaVersion().isEqualOrNewerThan(JavaVersion.TWENTY))
        }
    };

    /**
     * Determines whether the threading is active.
     *
     * @param environment the environment
     * @return whether the threading is active
     */
    abstract fun isActive(environment: Environment): Boolean
}
