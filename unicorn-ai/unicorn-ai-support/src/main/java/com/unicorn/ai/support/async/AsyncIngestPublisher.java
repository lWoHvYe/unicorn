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

import com.unicorn.ai.support.config.SupportProperties;
import com.unicorn.ai.support.knowledge.AsyncIngestResponse;
import com.unicorn.ai.support.knowledge.KnowledgeRepository;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import tools.jackson.databind.json.JsonMapper;

@Component
@ConditionalOnProperty(name = "ai.support.async-ingest.enabled", havingValue = "true")
public class AsyncIngestPublisher {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final RabbitTemplate rabbitTemplate;
    private final KnowledgeRepository repository;
    private final SupportProperties properties;

    public AsyncIngestPublisher(RabbitTemplate rabbitTemplate, KnowledgeRepository repository,
                                SupportProperties properties) {
        this.rabbitTemplate = rabbitTemplate;
        this.repository = repository;
        this.properties = properties;
    }

    public void publishDeadLetter(IngestCommand command, String error) {
        rabbitTemplate.convertAndSend(properties.asyncIngest().exchange() + ".dlx", "dead",
                JSON.writeValueAsString(java.util.Map.of("command", command,
                        "error", error == null ? "" : error)));
    }

    public AsyncIngestResponse publish(String jobId, String tenantId, String documentId, String versionId) {
        var command = new IngestCommand(jobId, tenantId, documentId, versionId);
        try {
            rabbitTemplate.convertAndSend(properties.asyncIngest().exchange(),
                    properties.asyncIngest().routingKey(), JSON.writeValueAsString(command));
            var attempts = repository.findJob(tenantId, jobId).map(job -> job.attempts()).orElse(0);
            repository.updateJob(jobId, "QUEUED", attempts, null);
            return new AsyncIngestResponse(jobId, "/api/ai/support/v1/jobs/" + jobId);
        } catch (RuntimeException exception) {
            repository.updateJob(jobId, "FAILED", 0, exception.getMessage());
            throw exception;
        }
    }
}
