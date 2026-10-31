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
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import tools.jackson.databind.json.JsonMapper;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs the same end-to-end path against the real dev services:
 * PostgreSQL/pgvector, Redis and RabbitMQ. Enabled explicitly with AI_INFRA_TEST=true.
 */
@EnabledIfEnvironmentVariable(named = "AI_INFRA_TEST", matches = "true")
@ActiveProfiles("infra")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class SupportExternalInfraTest {

    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final String SCHEMA = "ai_support_p2_test";
    private static final String DB_URL = env("AI_DB_URL",
            "jdbc:postgresql://10.211.55.29:5434/vectordb?currentSchema=" + SCHEMA + ",public");
    private static final String DB_USER = env("AI_DB_USER", "postgres");
    private static final String DB_PWD = env("AI_DB_PWD", "postgres");
    private static final String MQ_HOST = env("MQ_HOST", "10.211.55.29");
    private static final String MQ_PORT = env("MQ_PORT", "5672");
    private static final String MQ_USER = env("MQ_USER", "admin");
    private static final String MQ_PWD = env("MQ_PWD", "admin");
    private static final String MQ_VHOST = env("MQ_VHOST", "/unicorn");

    @DynamicPropertySource
    static void properties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", () -> DB_URL);
        registry.add("spring.datasource.username", () -> DB_USER);
        registry.add("spring.datasource.password", () -> DB_PWD);
        registry.add("spring.datasource.driver-class-name", () -> "org.postgresql.Driver");
        registry.add("spring.flyway.locations",
                () -> "classpath:db/migration/common,classpath:db/migration/postgres");
        registry.add("spring.flyway.schemas", () -> SCHEMA);
        registry.add("spring.flyway.default-schema", () -> SCHEMA);
        registry.add("spring.flyway.create-schemas", () -> true);
        registry.add("ai.support.pgvector.schema-name", () -> SCHEMA);
        registry.add("ai.support.vector-store", () -> "pgvector");
        registry.add("ai.support.demo.seed", () -> true);
        registry.add("ai.support.demo.model", () -> true);
        registry.add("ai.support.async-ingest.enabled", () -> true);
        registry.add("ai.support.rate-limit.store", () -> "redis");
        // This test may run repeatedly against the same Redis; avoid cross-run 429 noise.
        registry.add("ai.support.rate-limit.chat-per-minute", () -> 100000);
        registry.add("ai.support.rate-limit.ingest-per-hour", () -> 100000);
        registry.add("ai.support.rate-limit.mcp-per-minute", () -> 100000);
        registry.add("ai.support.cache.store", () -> "redis");
        registry.add("spring.rabbitmq.host", () -> MQ_HOST);
        registry.add("spring.rabbitmq.port", () -> MQ_PORT);
        registry.add("spring.rabbitmq.username", () -> MQ_USER);
        registry.add("spring.rabbitmq.password", () -> MQ_PWD);
        registry.add("spring.rabbitmq.virtual-host", () -> MQ_VHOST);
    }

    @LocalServerPort
    private int port;

    @Autowired
    private JdbcTemplate jdbc;

    @Autowired
    private StringRedisTemplate redis;

    private final HttpClient http = HttpClient.newHttpClient();

    @Test
    void externalInfraProcessesIngestionAndCitation() throws Exception {
        redis.delete(java.util.List.of("ai:support:rate:ingest:demo", "ai:support:rate:chat:demo",
                "ai:support:rate:mcp:demo"));
        var marker = java.util.UUID.randomUUID().toString();
        var ingest = post("/api/ai/support/v1/knowledge-bases/demo-kb/documents/text", """
                {"text":"External PG pgvector RabbitMQ 集成测试文档 %s，端口 8081。",
                 "source":"integration/external-%s.md","title":"External","docType":"faq"}
                """.formatted(marker, marker), "demo-key");
        assertEquals(200, ingest.statusCode());
        var body = JSON.readValue(ingest.body(), Map.class);
        assertTrue(body.get("jobId") != null, "ingest response: " + ingest.body());
        var jobId = body.get("jobId").toString();
        assertFalse(jobId.isBlank());

        var status = "";
        for (int i = 0; i < 40; i++) {
            var job = get("/api/ai/support/v1/jobs/" + jobId, "demo-key");
            var jobBody = JSON.readValue(job.body(), Map.class);
            status = String.valueOf(jobBody.get("status"));
            if ("COMPLETED".equals(status) || "DEAD".equals(status)) {
                break;
            }
            Thread.sleep(500);
        }
        assertEquals("COMPLETED", status);
        assertEquals(1, jdbc.queryForObject("""
                SELECT COUNT(*) FROM ai_outbox WHERE aggregate_id = ? AND status = 'PUBLISHED'
                """, Integer.class, jobId));

        var session = post("/api/ai/support/v1/sessions", "{\"channel\":\"web\"}", "demo-key");
        var sessionId = JSON.readValue(session.body(), Map.class).get("id");
        var chat = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"External PG pgvector " + marker + "\"}", "demo-key");
        assertEquals(200, chat.statusCode());
        var answer = JSON.readValue(chat.body(), Map.class);
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

    @Test
    void rlsIsolatesRowsForNonOwnerRole() throws Exception {
        var role = "ai_rls_test";
        jdbc.execute("""
                DO $$
                BEGIN
                    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'ai_rls_test') THEN
                        CREATE ROLE ai_rls_test LOGIN PASSWORD 'ai_rls_test_pwd';
                    END IF;
                END $$;
                """);
        jdbc.execute("ALTER ROLE ai_rls_test PASSWORD 'ai_rls_test_pwd'");
        jdbc.execute("GRANT CONNECT ON DATABASE vectordb TO ai_rls_test");
        jdbc.execute("GRANT USAGE ON SCHEMA " + SCHEMA + " TO ai_rls_test");
        jdbc.execute("GRANT SELECT ON ALL TABLES IN SCHEMA " + SCHEMA + " TO ai_rls_test");
        jdbc.update("""
                INSERT INTO ai_chat_session (id, tenant_id, user_id, channel, title)
                VALUES ('rls-demo', 'demo', 'rls-user', 'test', 'RLS demo')
                ON CONFLICT (id) DO NOTHING
                """);
        jdbc.update("""
                INSERT INTO ai_chat_session (id, tenant_id, user_id, channel, title)
                VALUES ('rls-other', 'other', 'rls-user', 'test', 'RLS other')
                ON CONFLICT (id) DO NOTHING
                """);
        try (Connection connection = DriverManager.getConnection(DB_URL, role, "ai_rls_test_pwd")) {
            assertEquals(List.of("demo"), tenantsVisibleTo(connection, "demo"));
            assertEquals(List.of("other"), tenantsVisibleTo(connection, "other"));
            assertTrue(tenantsVisibleTo(connection, "").isEmpty());
        } finally {
            jdbc.execute("DROP OWNED BY ai_rls_test");
            jdbc.execute("DROP ROLE IF EXISTS ai_rls_test");
            jdbc.update("DELETE FROM ai_chat_session WHERE id IN ('rls-demo', 'rls-other')");
        }
    }

    private List<String> tenantsVisibleTo(Connection connection, String tenant) throws Exception {
        try (var set = connection.prepareStatement("SELECT set_config('app.tenant_id', ?, false)")) {
            set.setString(1, tenant);
            set.execute();
        }
        try (var statement = connection.prepareStatement(
                "SELECT DISTINCT tenant_id FROM ai_chat_session ORDER BY tenant_id");
             var result = statement.executeQuery()) {
            var tenants = new java.util.ArrayList<String>();
            while (result.next()) {
                tenants.add(result.getString(1));
            }
            return tenants;
        }
    }

    private static String env(String key, String fallback) {
        var value = System.getenv(key);
        return value == null || value.isBlank() ? fallback : value;
    }
}
