/*
 *    Copyright (c) 2025.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.unicornopenai.rest;

import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.memory.ChatMemoryRepository;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.chat.prompt.Prompt;
import org.springframework.ai.deepseek.DeepSeekAssistantMessage;
import org.springframework.ai.deepseek.DeepSeekChatModel;
import org.springframework.ai.deepseek.DeepSeekChatOptions;
import org.springframework.ai.deepseek.api.DeepSeekApi;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
@RestController
public class WebController {

    private final ChatClient chatClient;

    @Autowired
    private DeepSeekChatModel chatModel;

    @Autowired
    ChatMemory chatMemory;

    @Autowired
    ChatMemoryRepository chatMemoryRepository;

    // 有时用chatClient，针对特定模型又常用对应的chatModel
    WebController(ChatClient.Builder clientBuilder) {
        this.chatClient = clientBuilder.build();
    }

    @GetMapping("/hello")
    public String hello(@RequestParam(value = "input", defaultValue = "讲一个笑话") String input) {
        return chatClient.prompt(input).call().content();
    }

    @GetMapping(value = "/hello/stream", produces = "text/html;charset=UTF-8")
    public Flux<String> helloStream(@RequestParam(value = "input", defaultValue = "讲一个笑话") String input) {
        return chatClient.prompt(input).stream().content();
    }

    private final ExecutorService executor = Executors.newFixedThreadPool(2);

    @GetMapping(value = "/mock/stream", produces = "text/html;charset=UTF-8")
    public Flux<String> mockStream() {
        Sinks.Many<String> sink = Sinks.many().multicast().onBackpressureBuffer();
        executor.submit(() -> {
            for (int i = 0; i < 100; i++) {
                sink.tryEmitNext(i + " ");
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        });
        return sink.asFlux();
    }

    @GetMapping("/reasoner")
    public String deepSeekReasonerMultiRoundExample() {
        List<Message> messages = new ArrayList<>();
        messages.add(new UserMessage("9.11 and 9.8, which is greater?"));
        DeepSeekChatOptions promptOptions = DeepSeekChatOptions.builder()
                .model(DeepSeekApi.ChatModel.DEEPSEEK_REASONER.getValue())
                .build();

        Prompt prompt = new Prompt(messages, promptOptions);
        ChatResponse response = chatModel.call(prompt);

        DeepSeekAssistantMessage deepSeekAssistantMessage = (DeepSeekAssistantMessage) response.getResult().getOutput();
        String reasoningContent = deepSeekAssistantMessage.getReasoningContent(); // 推理过程
        log.info("reasoningContent: {}", reasoningContent);
        String text = deepSeekAssistantMessage.getText(); // 结果输出

        messages.add(new AssistantMessage(Objects.requireNonNull(text)));
        messages.add(new UserMessage("How many Rs are there in the word 'strawberry'?"));
        Prompt prompt2 = new Prompt(messages, promptOptions);
        ChatResponse response2 = chatModel.call(prompt2);

        DeepSeekAssistantMessage deepSeekAssistantMessage2 = (DeepSeekAssistantMessage) response2.getResult().getOutput();
        String reasoningContent2 = deepSeekAssistantMessage2.getReasoningContent();
        log.info("reasoningContent2: {}", reasoningContent2);
        return deepSeekAssistantMessage2.getText();
    }
}

