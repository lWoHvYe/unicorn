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

package com.unicorn.ai.mcp.server.domain;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DemoDataServiceTest {

    private final DemoDataService data = new DemoDataService();

    @Test
    void orderLookup() {
        assertEquals("DELIVERED", data.order("A1001").status());
        assertEquals("NOT_FOUND", data.order("X-404").status());
    }

    @Test
    void refundEligibleWithinWindow() {
        assertTrue(data.refundEligibility("A1004").eligible());
    }

    @Test
    void refundRejectedOutsideWindowOrNotDelivered() {
        assertFalse(data.refundEligibility("A1001").eligible());
        assertFalse(data.refundEligibility("A1002").eligible());
    }

    @Test
    void refundUpdatesStatus() {
        assertTrue(data.refund("A1004").eligible());
        assertEquals("REFUNDED", data.order("A1004").status());
    }

    @Test
    void reserveInventory() {
        var reservation = data.reserve("SKU-1", 2);
        assertTrue(reservation.success());
        assertEquals(10, data.inventory("SKU-1").available());
        assertEquals(4, data.inventory("SKU-1").reserved());
    }

    @Test
    void rejectInsufficientStock() {
        assertFalse(data.reserve("SKU-2", 99).success());
        assertEquals(3, data.inventory("SKU-2").available());
    }
}
