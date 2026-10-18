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

package com.unicorn.ai.rag.async;

import com.unicorn.ai.rag.job.IngestJobRepository;
import com.unicorn.ai.rag.model.AsyncIngestResponse;
import com.unicorn.ai.rag.model.TextIngestRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import tools.jackson.databind.json.JsonMapper;

import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "demo.ai.rag.async-ingest.enabled", havingValue = "true")
public class AsyncIngestPublisher {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final RabbitTemplate rabbitTemplate;
    private final IngestJobRepository jobRepository;
    private final AsyncIngestProperties properties;

    public AsyncIngestResponse publish(TextIngestRequest request) {
        var jobId = UUID.randomUUID().toString();
        var source = StringUtils.hasText(request.source()) ? request.source() : "async-" + jobId;
        var command = new IngestCommand(jobId, new TextIngestRequest(
                request.text(), source, request.tenant(), request.docType()));

        jobRepository.createRunning(jobId, source);
        try {
            rabbitTemplate.convertAndSend(
                    properties.exchange(),
                    properties.routingKey(),
                    JSON.writeValueAsString(command));
        } catch (RuntimeException exception) {
            jobRepository.fail(jobId, exception.getMessage());
            throw exception;
        }
        log.info("Published async ingest job {} for source {}", jobId, source);
        return new AsyncIngestResponse(jobId, "/api/ai/rag/jobs/" + jobId);
    }
}
