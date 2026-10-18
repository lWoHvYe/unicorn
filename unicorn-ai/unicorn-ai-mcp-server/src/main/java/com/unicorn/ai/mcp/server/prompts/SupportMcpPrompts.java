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

package com.unicorn.ai.mcp.server.prompts;

import com.unicorn.ai.mcp.server.domain.DemoDataService;
import io.modelcontextprotocol.spec.McpSchema.GetPromptResult;
import io.modelcontextprotocol.spec.McpSchema.PromptMessage;
import io.modelcontextprotocol.spec.McpSchema.Role;
import io.modelcontextprotocol.spec.McpSchema.TextContent;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.mcp.annotation.McpArg;
import org.springframework.ai.mcp.annotation.McpPrompt;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class SupportMcpPrompts {

    private final DemoDataService data;

    @McpPrompt(name = "order-support", description = "根据订单状态生成客服回复草稿")
    public GetPromptResult orderSupport(
            @McpArg(name = "orderId", description = "订单号", required = true) String orderId,
            @McpArg(name = "tone", description = "语气：friendly/formal", required = false) String tone) {
        var order = data.order(orderId);
        var actualTone = tone == null || tone.isBlank() ? "friendly" : tone;
        var message = """
                你是一名电商客服，语气为 %s。
                订单信息：订单号 %s，客户 %s，状态 %s，金额 %s，下单日期 %s。
                请生成一段 80 字以内的客服回复，说明当前状态与下一步动作；不要承诺未确认的时效。
                """.formatted(actualTone, order.orderId(), order.customer(), order.status(),
                order.amount(), order.createdAt());
        return new GetPromptResult(
                "order-support",
                List.of(new PromptMessage(Role.ASSISTANT, new TextContent(message))));
    }
}
