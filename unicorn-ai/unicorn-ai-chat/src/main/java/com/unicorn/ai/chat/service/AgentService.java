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

package com.unicorn.ai.chat.service;

import com.unicorn.ai.chat.model.AgentRun;
import com.unicorn.ai.chat.model.ChatRequest;
import com.unicorn.ai.chat.tools.ShopTools;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

/**
 * 一个最小但完整的 agent 编排：
 * 1) 先用结构化输出生成计划；
 * 2) 再带着工具执行计划，由 Spring AI 的 ToolCallingAdvisor 驱动工具循环；
 * 3) 返回计划与最终答复，便于前端展示“思考/执行”过程。
 */
@Service
@RequiredArgsConstructor
public class AgentService {

    private final ChatClient chatClient;
    private final ShopTools shopTools;

    public AgentRun run(ChatRequest request) {
        if (request.message() == null || request.message().isBlank()) {
            throw new IllegalArgumentException("message 不能为空");
        }
        var conversationId = request.conversationIdOrDefault();

        var plan = chatClient.prompt()
                .user(user -> user.text("""
                                你是任务规划器。把目标拆成 2-4 步，并列出可能需要使用的工具名称。
                                可用工具：getOrder、refundEligibility、searchProducts、now、handoffToHuman。
                                目标：{goal}
                                只输出符合 AgentPlan schema 的内容。
                                """)
                        .param("goal", request.message()))
                .advisors(advisors -> advisors.param(ChatMemory.CONVERSATION_ID, conversationId))
                .call()
                .entity(AgentRun.AgentPlan.class);

        if (plan == null || plan.steps() == null || plan.steps().isEmpty()) {
            plan = new AgentRun.AgentPlan(List.of("直接依据已有信息回答"), List.of());
        }

        var planText = String.join("\n", plan.steps());
        var answer = chatClient.prompt()
                .user(user -> user.text("""
                                目标：{goal}
                                计划：
                                {plan}

                                请按计划执行。需要订单、退款或商品数据时必须调用工具；
                                最后用中文给出结论，并说明引用了哪些工具结果。
                                """)
                        .param("goal", request.message())
                        .param("plan", planText))
                .tools(shopTools)
                .toolContext(Map.of("tenant", "demo"))
                .advisors(advisors -> advisors.param(ChatMemory.CONVERSATION_ID, conversationId))
                .call()
                .content();

        return new AgentRun(request.message(), plan, answer);
    }
}
