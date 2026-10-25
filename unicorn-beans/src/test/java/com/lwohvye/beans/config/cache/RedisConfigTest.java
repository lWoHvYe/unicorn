/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.beans.config.cache;

import org.junit.jupiter.api.Test;
import org.springframework.data.redis.serializer.JacksonJsonRedisSerializer;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

class RedisConfigTest {

    private final JacksonJsonRedisSerializer<Object> serializer = new RedisConfig().jacksonJsonRedisSerializer();

    @Test
    void roundTripsList() {
        var value = new ArrayList<>(List.of("a", "b"));

        var result = serializer.deserialize(serializer.serialize(value));

        assertInstanceOf(List.class, result);
        assertEquals(value, result);
    }

    @Test
    void roundTripsMap() {
        var value = new LinkedHashMap<String, Object>();
        value.put("name", "unicorn");
        value.put("count", 2);

        var result = serializer.deserialize(serializer.serialize(value));

        assertInstanceOf(Map.class, result);
        assertEquals(value, result);
    }

    @Test
    void roundTripsPojoInList() {
        var value = new ArrayList<>(List.of(new Demo("unicorn", 2)));

        var result = serializer.deserialize(serializer.serialize(value));

        assertInstanceOf(List.class, result);
        assertEquals(value, result);
    }

    static class Demo {
        public String name;
        public int count;

        public Demo() {
        }

        public Demo(String name, int count) {
            this.name = name;
            this.count = count;
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof Demo demo && count == demo.count && java.util.Objects.equals(name, demo.name);
        }

        @Override
        public int hashCode() {
            return java.util.Objects.hash(name, count);
        }
    }
}
