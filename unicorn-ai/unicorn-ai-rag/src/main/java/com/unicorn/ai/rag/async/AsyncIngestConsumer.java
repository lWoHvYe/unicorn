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

import com.unicorn.ai.rag.ingestion.DocumentIngestionService;
import com.unicorn.ai.rag.job.IngestJobRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import tools.jackson.databind.json.JsonMapper;

@Slf4j
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "demo.ai.rag.async-ingest.enabled", havingValue = "true")
public class AsyncIngestConsumer {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final DocumentIngestionService ingestionService;
    private final IngestJobRepository jobRepository;

    @RabbitListener(queues = "${demo.ai.rag.async-ingest.queue}")
    public void consume(String payload) {
        final IngestCommand command;
        try {
            command = JSON.readValue(payload, IngestCommand.class);
        } catch (RuntimeException exception) {
            log.error("Invalid async ingest payload: {}", payload, exception);
            return;
        }

        try {
            var result = ingestionService.ingestText(command.request());
            jobRepository.complete(command.jobId(), result.chunks());
            log.info("Async ingest job {} completed with {} chunks", command.jobId(), result.chunks());
        } catch (RuntimeException exception) {
            jobRepository.fail(command.jobId(), exception.getMessage());
            log.error("Async ingest job {} failed", command.jobId(), exception);
        }
    }
}
