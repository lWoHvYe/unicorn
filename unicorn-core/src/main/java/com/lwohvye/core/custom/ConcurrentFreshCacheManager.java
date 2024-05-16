/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.custom;

import org.jetbrains.annotations.NotNull;
import org.springframework.cache.Cache;
import org.springframework.cache.concurrent.ConcurrentMapCache;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;

/**
 * CustomLocalCacheManager with fresh cache
 */
public class ConcurrentFreshCacheManager extends ConcurrentMapCacheManager {
    @Override
    protected @NotNull Cache createConcurrentMapCache(@NotNull String name) {
        return new ConcurrentMapCache(name, new ConcurrentFreshMap<>(256), isAllowNullValues());
    }
}
