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

import com.lwohvye.core.utils.json.JsonUtils;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

// only override get/put for some specific scenario (need fresh value)
@NoArgsConstructor
@SuppressWarnings("unchecked")
public class ConcurrentFreshMap<K, V> extends ConcurrentHashMap<K, V> {

    public ConcurrentFreshMap(int initialCapacity) {
        super(initialCapacity);
    }

    @Override
    public V get(Object key) {
        var value = super.get(key);
        if (Objects.nonNull(value)) return JsonUtils.deepCopy(value, (Class<V>) value.getClass());
        else return null;
    }

    @Override
    public V put(@NotNull K key, @NotNull V value) {
        return super.put(key, JsonUtils.deepCopy(value, (Class<V>) value.getClass()));
    }
}
