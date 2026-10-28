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

package com.unicorn.ai.support;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.RabbitMQContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import tools.jackson.databind.json.JsonMapper;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Testcontainers(disabledWithoutDocker = true)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class SupportInfraIntegrationTest {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    @Container
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("pgvector/pgvector:pg17")
            .withDatabaseName("vectordb")
            .withUsername("postgres")
            .withPassword("postgres");

    @Container
    static final RabbitMQContainer RABBIT = new RabbitMQContainer("rabbitmq:4-management")
            .withUser("admin", "admin")
            .withVhost("/unicorn");

    @DynamicPropertySource
    static void properties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", () -> POSTGRES.getJdbcUrl() + "?currentSchema=ai_support");
        registry.add("spring.datasource.username", POSTGRES::getUsername);
        registry.add("spring.datasource.password", POSTGRES::getPassword);
        registry.add("spring.datasource.driver-class-name", () -> "org.postgresql.Driver");
        registry.add("spring.datasource.hikari.schema", () -> "ai_support");
        registry.add("spring.flyway.locations",
                () -> "classpath:db/migration/common,classpath:db/migration/postgres");
        registry.add("spring.flyway.schemas", () -> "ai_support");
        registry.add("spring.flyway.default-schema", () -> "ai_support");
        registry.add("spring.flyway.create-schemas", () -> true);
        registry.add("ai.support.vector-store", () -> "pgvector");
        registry.add("ai.support.demo.seed", () -> true);
        registry.add("ai.support.demo.model", () -> true);
        registry.add("ai.support.async-ingest.enabled", () -> true);
        registry.add("spring.rabbitmq.host", RABBIT::getHost);
        registry.add("spring.rabbitmq.port", RABBIT::getAmqpPort);
        registry.add("spring.rabbitmq.username", () -> "admin");
        registry.add("spring.rabbitmq.password", () -> "admin");
        registry.add("spring.rabbitmq.virtual-host", () -> "/unicorn");
        registry.add("management.health.rabbit.enabled", () -> false);
    }

    @LocalServerPort
    private int port;

    private final HttpClient http = HttpClient.newHttpClient();

    @Test
    void postgresqlAndRabbitMqProcessIngestionAndCitation() throws Exception {
        var ingest = post("/api/ai/support/v1/knowledge-bases/demo-kb/documents/text", """
                {"text":"PostgreSQL pgvector 集成测试文档，端口 8081。",
                 "source":"integration/pgvector.md","title":"PG","docType":"faq"}
                """, "demo-key");
        assertEquals(200, ingest.statusCode());
        var body = JSON.readValue(ingest.body(), java.util.Map.class);
        var jobId = body.get("jobId").toString();
        assertFalse(jobId.isBlank());

        var status = "";
        for (int i = 0; i < 40; i++) {
            var job = get("/api/ai/support/v1/jobs/" + jobId, "demo-key");
            var jobBody = JSON.readValue(job.body(), java.util.Map.class);
            status = String.valueOf(jobBody.get("status"));
            if ("COMPLETED".equals(status) || "DEAD".equals(status)) {
                break;
            }
            Thread.sleep(500);
        }
        assertEquals("COMPLETED", status);

        var session = post("/api/ai/support/v1/sessions", "{\"channel\":\"web\"}", "demo-key");
        var sessionId = JSON.readValue(session.body(), java.util.Map.class).get("id");
        var chat = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"PostgreSQL pgvector\"}", "demo-key");
        assertEquals(200, chat.statusCode());
        var answer = JSON.readValue(chat.body(), java.util.Map.class);
        assertTrue(((java.util.List<?>) answer.get("citations")).size() > 0);
    }

    private HttpResponse<String> post(String path, String body, String key) throws Exception {
        return http.send(HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("Content-Type", "application/json")
                .header("X-API-Key", key)
                .POST(HttpRequest.BodyPublishers.ofString(body)).build(), HttpResponse.BodyHandlers.ofString());
    }

    private HttpResponse<String> get(String path, String key) throws Exception {
        return http.send(HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("X-API-Key", key).GET().build(), HttpResponse.BodyHandlers.ofString());
    }
}
