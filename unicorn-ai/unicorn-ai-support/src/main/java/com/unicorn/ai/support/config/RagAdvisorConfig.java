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

import com.unicorn.ai.support.retrieval.SupportDocumentRetriever;
import com.unicorn.ai.support.retrieval.SupportQueryAugmenter;
import org.springframework.ai.rag.advisor.RetrievalAugmentationAdvisor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.support.ContextPropagatingTaskDecorator;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
public class RagAdvisorConfig {

    @Bean
    RetrievalAugmentationAdvisor retrievalAugmentationAdvisor(SupportDocumentRetriever retriever,
                                                               SupportQueryAugmenter augmenter) {
        var executor = new ThreadPoolTaskExecutor();
        executor.setThreadNamePrefix("ai-rag-");
        executor.setCorePoolSize(4);
        executor.setMaxPoolSize(16);
        executor.setTaskDecorator(new ContextPropagatingTaskDecorator());
        executor.initialize();
        return RetrievalAugmentationAdvisor.builder()
                .documentRetriever(retriever)
                .queryAugmenter(augmenter)
                .taskExecutor(executor)
                .order(0)
                .build();
    }
}
