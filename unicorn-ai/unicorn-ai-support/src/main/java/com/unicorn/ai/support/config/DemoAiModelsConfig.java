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

package com.unicorn.ai.support.config;

import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.metadata.ChatResponseMetadata;
import org.springframework.ai.chat.metadata.DefaultUsage;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.chat.model.Generation;
import org.springframework.ai.chat.prompt.Prompt;
import org.springframework.ai.document.Document;
import org.springframework.ai.embedding.Embedding;
import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.embedding.EmbeddingRequest;
import org.springframework.ai.embedding.EmbeddingResponse;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * Offline models for local demo and deterministic tests. The infra profile disables this bean
 * and uses the configured OpenAI-compatible provider instead.
 */
@Configuration
@ConditionalOnProperty(name = "ai.support.demo.model", havingValue = "true", matchIfMissing = true)
public class DemoAiModelsConfig {

    private static final int DIMENSIONS = 1536;

    @Bean
    @Primary
    ChatModel demoChatModel() {
        return new DemoChatModel();
    }

    @Bean
    @Primary
    EmbeddingModel demoEmbeddingModel() {
        return new DemoEmbeddingModel(DIMENSIONS);
    }

    static class DemoChatModel implements ChatModel {

        @Override
        public ChatResponse call(Prompt prompt) {
            var userMessage = prompt.getUserMessage();
            var text = userMessage == null ? null : userMessage.getText();
            var answer = "【本地演示模型】已收到问题。生产环境请配置 OPENAI_API_KEY。\n问题摘要："
                    + (text == null ? "" : text.substring(0, Math.min(text.length(), 200)));
            var metadata = ChatResponseMetadata.builder()
                    .usage(new DefaultUsage(10, 20, 30))
                    .build();
            return new ChatResponse(List.of(new Generation(new AssistantMessage(answer))), metadata);
        }

        @Override
        public Flux<ChatResponse> stream(Prompt prompt) {
            return Flux.just(call(prompt));
        }
    }

    static class DemoEmbeddingModel implements EmbeddingModel {

        private final int dimensions;

        DemoEmbeddingModel(int dimensions) {
            this.dimensions = dimensions;
        }

        @Override
        public EmbeddingResponse call(EmbeddingRequest request) {
            var embeddings = new ArrayList<Embedding>();
            var instructions = request.getInstructions();
            for (int i = 0; i < instructions.size(); i++) {
                embeddings.add(new Embedding(vector(instructions.get(i)), i));
            }
            return new EmbeddingResponse(embeddings);
        }

        @Override
        public float[] embed(Document document) {
            return vector(document.getText());
        }

        @Override
        public int dimensions() {
            return dimensions;
        }

        private float[] vector(String text) {
            var vector = new float[dimensions];
            if (text == null || text.isBlank()) {
                vector[0] = 1f;
                return vector;
            }
            var bytes = text.getBytes(StandardCharsets.UTF_8);
            for (int i = 0; i < bytes.length; i++) {
                var index = Math.floorMod((bytes[i] & 0xff) * 31 + i, dimensions);
                vector[index] += 1f;
            }
            var norm = 0f;
            for (var value : vector) {
                norm += value * value;
            }
            norm = (float) Math.sqrt(norm);
            if (norm > 0) {
                for (int i = 0; i < vector.length; i++) {
                    vector[i] /= norm;
                }
            }
            return vector;
        }
    }
}
