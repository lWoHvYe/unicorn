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

package com.unicorn.ai.tools.domain;

import com.unicorn.ai.tools.model.Product;

import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class InMemoryProductRepository implements ProductRepository {

    private final Map<String, List<Product>> products = new ConcurrentHashMap<>();

    public InMemoryProductRepository() {
        seed("demo");
    }

    public void seed(String tenantId) {
        products.put(tenantId, List.of(
                new Product(tenantId, "SKU-1", "机械键盘", new BigDecimal("499.00"), 12),
                new Product(tenantId, "SKU-2", "人体工学椅", new BigDecimal("1999.00"), 3),
                new Product(tenantId, "SKU-3", "USB-C 扩展坞", new BigDecimal("299.00"), 30)));
    }

    @Override
    public List<Product> search(String tenantId, String keyword) {
        var lowered = keyword == null ? "" : keyword.toLowerCase(Locale.ROOT);
        return products.getOrDefault(tenantId, List.of()).stream()
                .filter(product -> product.name().toLowerCase(Locale.ROOT).contains(lowered)
                        || product.sku().toLowerCase(Locale.ROOT).contains(lowered))
                .toList();
    }
}
