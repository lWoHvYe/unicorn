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

package com.unicorn.ai.chat.tools;

import org.junit.jupiter.api.Test;
import org.springframework.ai.chat.model.ToolContext;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShopToolsTest {

    private final ShopTools tools = new ShopTools();
    private final ToolContext context = new ToolContext(Map.of("tenant", "acme"));

    @Test
    void orderCarriesTenant() {
        var order = tools.getOrder("A1001", context);
        assertEquals("DELIVERED", order.status());
        assertEquals("acme", order.tenant());
    }

    @Test
    void missingOrderReturnsNotFound() {
        assertEquals("NOT_FOUND", tools.getOrder("X-404", context).status());
    }

    @Test
    void refundRejectedOutsideWindow() {
        assertFalse(tools.refundEligibility("A1001", context).eligible());
    }

    @Test
    void refundAcceptedWithinWindow() {
        assertTrue(tools.refundEligibility("A1004", context).eligible());
    }

    @Test
    void searchProductsByKeyword() {
        var products = tools.searchProducts("键盘");
        assertEquals(1, products.size());
        assertEquals("SKU-1", products.getFirst().sku());
    }

    @Test
    void handoffReturnsDirectMessage() {
        assertTrue(tools.handoffToHuman("需要人工").contains("人工"));
    }
}
