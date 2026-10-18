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

package com.unicorn.ai.mcp.server.tools;

import com.unicorn.ai.mcp.server.domain.DemoDataService;
import com.unicorn.ai.mcp.server.domain.DemoDataService.InventoryItem;
import com.unicorn.ai.mcp.server.domain.DemoDataService.Reservation;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.mcp.annotation.McpTool;
import org.springframework.ai.mcp.annotation.McpToolParam;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InventoryMcpTools {

    private final DemoDataService data;

    @McpTool(name = "check_inventory", description = "按 SKU 查询可用库存与预留库存",
            annotations = @McpTool.McpAnnotations(readOnlyHint = true, destructiveHint = false,
                    openWorldHint = false))
    public InventoryItem checkInventory(
            @McpToolParam(description = "SKU，例如 SKU-1", required = true) String sku) {
        return data.inventory(sku);
    }

    @McpTool(name = "reserve_inventory", description = "按 SKU 预留指定数量库存（写操作）",
            annotations = @McpTool.McpAnnotations(destructiveHint = true, idempotentHint = false,
                    openWorldHint = false))
    public Reservation reserveInventory(
            @McpToolParam(description = "SKU，例如 SKU-1", required = true) String sku,
            @McpToolParam(description = "预留数量", required = true) int quantity) {
        return data.reserve(sku, quantity);
    }
}
