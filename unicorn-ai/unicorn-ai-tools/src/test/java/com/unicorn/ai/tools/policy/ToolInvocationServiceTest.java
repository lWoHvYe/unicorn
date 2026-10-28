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

package com.unicorn.ai.tools.policy;

import com.unicorn.ai.tools.domain.InMemoryInventoryRepository;
import com.unicorn.ai.tools.domain.InMemoryOrderRepository;
import com.unicorn.ai.tools.domain.InMemoryProductRepository;
import com.unicorn.ai.tools.domain.SupportToolService;
import com.unicorn.ai.tools.model.ToolExecutionContext;
import com.unicorn.ai.tools.model.ToolStatus;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolInvocationServiceTest {

    private ToolInvocationService service;

    @BeforeEach
    void setUp() {
        var tools = new SupportToolService(new InMemoryOrderRepository(),
                new InMemoryInventoryRepository(), new InMemoryProductRepository());
        service = new ToolInvocationService(new InMemoryToolInvocationRepository());
        SupportToolCatalog.register(service, tools, new BigDecimal("500.00"));
    }

    @Test
    void readToolIsAuditedAndReplayable() {
        var context = context(Set.of("SUPPORT_AGENT"), "read-1");
        var first = service.execute(context, "get_order", Map.of("tenantId", "demo", "orderId", "A1001"));
        assertEquals(ToolStatus.COMPLETED, first.status());

        var replay = service.execute(context, "get_order", Map.of("tenantId", "demo", "orderId", "A1001"));
        assertEquals(ToolStatus.COMPLETED, replay.status());
    }

    @Test
    void highValueWriteNeedsApprovalAndApprovalExecutes() {
        var context = context(Set.of("SUPPORT_MANAGER"), "write-1");
        var pending = service.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.PENDING_APPROVAL, pending.status());
        assertNotNull(pending.approvalId());

        var approved = service.approve("demo", pending.approvalId(), "manager", Set.of("SUPPORT_MANAGER"));
        assertEquals(ToolStatus.COMPLETED, approved.status());

        var replay = service.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.COMPLETED, replay.status());
    }

    @Test
    void writeToolRejectsMissingRole() {
        var context = context(Set.of("CUSTOMER"), "write-2");
        var result = service.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("100.00")));
        assertEquals(ToolStatus.FAILED, result.status());
    }

    @Test
    void tenantCannotReadAnotherTenantOrder() {
        var context = new ToolExecutionContext("other", "u", Set.of("SUPPORT_AGENT"),
                "s", "read-other", "trace");
        var result = service.execute(context, "get_order", Map.of("tenantId", "other", "orderId", "A1001"));
        assertEquals(ToolStatus.COMPLETED, result.status());
        assertTrue(result.data() instanceof Map<?, ?> map && "NOT_FOUND".equals(map.get("status")));
    }

    @Test
    void idempotencyKeyIsScopedToTool() {
        var context = context(Set.of("SUPPORT_MANAGER"), "shared-key");
        var order = service.execute(context, "get_order",
                Map.of("tenantId", "demo", "orderId", "A1004"));
        assertEquals(ToolStatus.COMPLETED, order.status());

        var refund = service.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.PENDING_APPROVAL, refund.status());
    }

    @Test
    void anotherTenantCannotApprove() {
        var context = context(Set.of("SUPPORT_MANAGER"), "write-3");
        var pending = service.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        org.junit.jupiter.api.Assertions.assertThrows(java.util.NoSuchElementException.class,
                () -> service.approve("other", pending.approvalId(), "manager", Set.of("SUPPORT_MANAGER")));
    }

    private ToolExecutionContext context(Set<String> roles, String idempotencyKey) {
        return new ToolExecutionContext("demo", "user", roles, "session", idempotencyKey, "trace");
    }
}
