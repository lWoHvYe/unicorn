/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.hiddenclass;


import lombok.SneakyThrows;

/**
 * Used as template for ASMifier.
 */
public class CustomerPropertyAccessorTemplate implements PropertyAccessor<Customer> {

    @SneakyThrows
    @Override
    // 该类并未加载，实际访问的为ASM的代理。如注释所言，这个是一个 template for ASMifier
    // java --add-opens java.base/jdk.internal.org.objectweb.asm.util=ALL-UNNAMED jdk.internal.org.objectweb.asm.util.ASMifier CustomerPropertyAccessorTemplate.class
    public Object getValue(Customer instance, String property) {
        // var field = ReflectionUtils.findField(instance.getClass(), property);
        // Objects.requireNonNull(field).trySetAccessible();
        // return field.get(instance);
        var field = instance.getClass().getDeclaredField(property);
        field.trySetAccessible();
        return field.get(instance);
        // if (property.equals("name")) {
        //     return instance.getName();
        // }
        //
        // if (property.equals("birthday")) {
        //     return instance.getBirthday();
        // }
        //
        // if (property.equals("address")) {
        //     return instance.getAddress();
        // }
        //
        // throw new IllegalArgumentException("Unknown property: " + property);
    }
}
