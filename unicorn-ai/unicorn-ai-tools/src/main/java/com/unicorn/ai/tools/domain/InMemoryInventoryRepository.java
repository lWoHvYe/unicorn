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

import com.unicorn.ai.tools.model.InventoryItem;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class InMemoryInventoryRepository implements InventoryRepository {

    private final Map<String, InventoryItem> inventory = new ConcurrentHashMap<>();

    public InMemoryInventoryRepository() {
        seed("demo");
    }

    public void seed(String tenantId) {
        inventory.put(key(tenantId, "SKU-1"), new InventoryItem(tenantId, "SKU-1", "机械键盘", 12, 2));
        inventory.put(key(tenantId, "SKU-2"), new InventoryItem(tenantId, "SKU-2", "人体工学椅", 3, 0));
        inventory.put(key(tenantId, "SKU-3"), new InventoryItem(tenantId, "SKU-3", "USB-C 扩展坞", 30, 5));
    }

    @Override
    public Optional<InventoryItem> find(String tenantId, String sku) {
        return Optional.ofNullable(inventory.get(key(tenantId, sku)));
    }

    @Override
    public InventoryItem save(InventoryItem item) {
        inventory.put(key(item.tenantId(), item.sku()), item);
        return item;
    }

    @Override
    public synchronized boolean reserve(String tenantId, String sku, int quantity) {
        var current = inventory.get(key(tenantId, sku));
        if (current == null || !current.canReserve(quantity)) {
            return false;
        }
        inventory.put(key(tenantId, sku), current.reserved(quantity));
        return true;
    }

    @Override
    public List<InventoryItem> findByTenant(String tenantId) {
        return inventory.entrySet().stream()
                .filter(entry -> entry.getKey().startsWith(tenantId + ":"))
                .map(Map.Entry::getValue)
                .toList();
    }

    private String key(String tenantId, String sku) {
        return tenantId + ":" + sku;
    }
}
