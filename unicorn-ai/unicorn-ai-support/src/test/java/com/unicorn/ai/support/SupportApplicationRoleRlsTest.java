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

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.boot.test.web.server.LocalServerPort;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.sql.DriverManager;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Starts the full application with a non-owner PostgreSQL role. This is the check
 * that proves RLS is active in the application connection path, not only in a
 * hand-written JDBC test.
 */
@EnabledIfEnvironmentVariable(named = "AI_INFRA_TEST", matches = "true")
@ActiveProfiles("infra")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class SupportApplicationRoleRlsTest {

    private static final String SCHEMA = "ai_support_app_role_test";
    private static final String DB_URL = env("AI_DB_URL",
            "jdbc:postgresql://10.211.55.29:5434/vectordb?currentSchema=" + SCHEMA + ",public");
    private static final String ADMIN_URL = env("AI_DB_URL",
            "jdbc:postgresql://10.211.55.29:5434/vectordb");
    private static final String DB_USER = env("AI_DB_USER", "postgres");
    private static final String DB_PWD = env("AI_DB_PWD", "postgres");
    private static final String APP_ROLE = "unicorn_support_app";
    private static final String APP_PWD = "unicorn_support_app_pwd";

    @DynamicPropertySource
    static void properties(DynamicPropertyRegistry registry) {
        prepareSchemaAndRole();
        registry.add("spring.datasource.url", () -> DB_URL);
        registry.add("spring.datasource.username", () -> APP_ROLE);
        registry.add("spring.datasource.password", () -> APP_PWD);
        registry.add("spring.datasource.driver-class-name", () -> "org.postgresql.Driver");
        registry.add("spring.flyway.enabled", () -> false);
        registry.add("ai.support.pgvector.schema-name", () -> SCHEMA);
        registry.add("ai.support.vector-store", () -> "simple");
        registry.add("ai.support.demo.seed", () -> true);
        registry.add("ai.support.demo.model", () -> true);
        registry.add("ai.support.async-ingest.enabled", () -> false);
        registry.add("ai.support.rate-limit.store", () -> "local");
        registry.add("ai.support.cache.store", () -> "local");
        registry.add("ai.support.rls.enabled", () -> true);
        registry.add("ai.support.rls.fail-fast", () -> true);
    }

    @LocalServerPort
    private int port;

    private final HttpClient http = HttpClient.newHttpClient();

    @Test
    void applicationStartsAndChatsWithNonOwnerRole() throws Exception {
        var session = post("/api/ai/support/v1/sessions", "{\"channel\":\"web\"}");
        assertEquals(200, session.statusCode(), session.body());
        var sessionId = Json.readString(session.body(), "id");
        var chat = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"Spring Boot 4.1\"}");
        assertEquals(200, chat.statusCode(), chat.body());
        assertTrue(chat.body().contains("answer") || chat.body().contains("citations"), chat.body());
    }

    private HttpResponse<String> post(String path, String body) throws Exception {
        var request = HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("Content-Type", "application/json")
                .header("X-API-Key", "demo-key")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build();
        return http.send(request, HttpResponse.BodyHandlers.ofString());
    }

    private static void prepareSchemaAndRole() {
        try (var connection = DriverManager.getConnection(ADMIN_URL, DB_USER, DB_PWD);
             var statement = connection.createStatement()) {
            // Extensions live in public so GIN operator classes resolve during Flyway.
            statement.execute("CREATE EXTENSION IF NOT EXISTS vector WITH SCHEMA public");
            // pg_trgm is installed by the external infra schema; creating it in another
            // schema would make gin_trgm_ops unavailable to Flyway's search path.
        } catch (Exception exception) {
            throw new IllegalStateException("Could not prepare PostgreSQL extensions", exception);
        }
        // Flyway must run as the admin role; the application itself runs as APP_ROLE.
        Flyway.configure()
                .dataSource(ADMIN_URL, DB_USER, DB_PWD)
                .schemas(SCHEMA)
                .defaultSchema(SCHEMA)
                .createSchemas(true)
                .locations("classpath:db/migration/common", "classpath:db/migration/postgres")
                .load()
                .migrate();
        try (var connection = DriverManager.getConnection(ADMIN_URL, DB_USER, DB_PWD);
             var statement = connection.createStatement()) {
            statement.execute("""
                    DO $$ BEGIN
                      IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = '%s') THEN
                        CREATE ROLE %s LOGIN PASSWORD '%s';
                      END IF;
                    END $$;
                    """.formatted(APP_ROLE, APP_ROLE, APP_PWD));
            statement.execute("GRANT CREATE ON DATABASE " + connection.getCatalog() + " TO " + APP_ROLE);
            statement.execute("GRANT USAGE, CREATE ON SCHEMA " + SCHEMA + " TO " + APP_ROLE);
            statement.execute("GRANT USAGE ON SCHEMA public TO " + APP_ROLE);
            statement.execute("GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA " + SCHEMA
                    + " TO " + APP_ROLE);
            statement.execute("GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA " + SCHEMA
                    + " TO " + APP_ROLE);
        } catch (Exception exception) {
            throw new IllegalStateException("Could not prepare application-role RLS test", exception);
        }
    }

    private static String env(String name, String fallback) {
        return System.getenv().getOrDefault(name, fallback);
    }

    private static final class Json {
        private static String readString(String json, String field) {
            var marker = "\"" + field + "\"";
            var index = json.indexOf(marker);
            if (index < 0) {
                throw new IllegalStateException("Missing field " + field + " in " + json);
            }
            var colon = json.indexOf(':', index + marker.length());
            var start = json.indexOf('"', colon + 1);
            var end = json.indexOf('"', start + 1);
            return json.substring(start + 1, end);
        }
    }
}
