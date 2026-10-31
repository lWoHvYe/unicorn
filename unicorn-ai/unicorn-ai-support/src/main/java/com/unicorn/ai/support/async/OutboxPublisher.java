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
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@ConditionalOnProperty(name = "ai.support.async-ingest.enabled", havingValue = "true")
public class OutboxPublisher {

    private final OutboxRepository repository;
    private final RabbitTemplate rabbitTemplate;
    private final SupportProperties properties;
    private final String owner = java.util.UUID.randomUUID().toString();

    public OutboxPublisher(OutboxRepository repository, RabbitTemplate rabbitTemplate,
                           SupportProperties properties) {
        this.repository = repository;
        this.rabbitTemplate = rabbitTemplate;
        this.properties = properties;
    }

    @Scheduled(fixedDelayString = "${ai.support.outbox.poll-ms:1000}")
    public void publishPending() {
        for (var event : repository.claim(owner, 20)) {
            try {
                if ("INGEST_REQUESTED".equals(event.eventType())) {
                    rabbitTemplate.convertAndSend(properties.asyncIngest().exchange(),
                            properties.asyncIngest().routingKey(), event.payloadJson());
                } else {
                    log.warn("Unknown outbox event type: {}", event.eventType());
                }
                repository.published(event.id(), owner);
            } catch (RuntimeException exception) {
                repository.failed(event.id(), owner, event.attempts());
                log.warn("Outbox event {} publish failed, retry scheduled", event.id(), exception);
            }
        }
    }
}
