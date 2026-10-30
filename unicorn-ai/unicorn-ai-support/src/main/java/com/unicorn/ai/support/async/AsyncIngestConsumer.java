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

package com.unicorn.ai.support.async;

import com.unicorn.ai.support.knowledge.DocumentIngestionService;
import com.unicorn.ai.support.knowledge.KnowledgeRepository;
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.security.TenantContext;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.AmqpRejectAndDontRequeueException;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import tools.jackson.databind.json.JsonMapper;

@Slf4j
@Component
@ConditionalOnProperty(name = "ai.support.async-ingest.enabled", havingValue = "true")
public class AsyncIngestConsumer {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final DocumentIngestionService ingestionService;
    private final KnowledgeRepository repository;
    private final AsyncIngestPublisher publisher;

    public AsyncIngestConsumer(DocumentIngestionService ingestionService, KnowledgeRepository repository,
                               AsyncIngestPublisher publisher) {
        this.ingestionService = ingestionService;
        this.repository = repository;
        this.publisher = publisher;
    }

    @RabbitListener(queues = "${ai.support.async-ingest.queue}")
    public void consume(String payload) {
        final IngestCommand command;
        try {
            command = JSON.readValue(payload, IngestCommand.class);
        } catch (RuntimeException exception) {
            log.error("Invalid ingest command: {}", payload, exception);
            throw new AmqpRejectAndDontRequeueException("Invalid ingest command", exception);
        }
        try {
            TenantContext.run(new AiPrincipal(command.tenantId(), "ingest-worker", java.util.Set.of("SYSTEM")),
                    () -> ingestionService.process(command.tenantId(), command.documentId(),
                            command.versionId(), command.jobId()));
            log.info("Ingest job {} completed", command.jobId());
        } catch (RuntimeException exception) {
            var attempts = repository.findJob(command.tenantId(), command.jobId())
                    .map(job -> job.attempts()).orElse(1);
            if (attempts < 3) {
                repository.updateJob(command.jobId(), "PENDING", attempts, exception.getMessage());
                publisher.publish(command.jobId(), command.tenantId(), command.documentId(), command.versionId());
                log.warn("Ingest job {} failed, retry {}/3", command.jobId(), attempts);
            } else {
                repository.updateJob(command.jobId(), "DEAD", attempts, exception.getMessage());
                publisher.publishDeadLetter(command, exception.getMessage());
                log.error("Ingest job {} exhausted retries", command.jobId(), exception);
            }
        }
    }
}
