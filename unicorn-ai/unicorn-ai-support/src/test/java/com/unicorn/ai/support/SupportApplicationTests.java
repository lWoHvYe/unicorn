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

import com.unicorn.ai.tools.model.ToolExecutionContext;
import com.unicorn.ai.tools.model.ToolStatus;
import com.unicorn.ai.tools.policy.ToolInvocationService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import tools.jackson.databind.json.JsonMapper;

import java.math.BigDecimal;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class SupportApplicationTests {

    private static final JsonMapper JSON = JsonMapper.builder().build();

    @LocalServerPort
    private int port;

    private final HttpClient http = HttpClient.newHttpClient();

    @Autowired
    private ToolInvocationService toolInvocations;

    @Test
    void ingestAndChatReturnCitation() {
        var ingest = post("/api/ai/support/v1/knowledge-bases/demo-kb/documents/text", """
                {"text":"Spring Boot 4.1 是当前支持的版本。Unicorn 默认端口 8081。",
                 "source":"test/faq.md","title":"FAQ","docType":"faq"}
                """, "demo-key");
        assertEquals(HttpStatus.OK.value(), ingest.statusCode());
        var ingestBody = JSON.readValue(ingest.body(), Map.class);
        assertEquals("COMPLETED", ingestBody.get("status"));

        var session = post("/api/ai/support/v1/sessions",
                "{\"channel\":\"web\",\"title\":\"test\"}", "demo-key");
        assertEquals(HttpStatus.OK.value(), session.statusCode());
        var sessionId = JSON.readValue(session.body(), Map.class).get("id");

        var chat = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"Spring Boot 4.1\"}", "demo-key");
        assertEquals(HttpStatus.OK.value(), chat.statusCode());
        var body = JSON.readValue(chat.body(), Map.class);
        assertEquals("DONE", body.get("status"));
        assertFalse(((java.util.List<?>) body.get("citations")).isEmpty());
        assertTrue(body.get("answer").toString().contains("本地演示模型"));
    }

    @Test
    void missingApiKeyIsUnauthorized() {
        var response = get("/api/ai/support/v1/knowledge-bases", "wrong-key");
        assertEquals(HttpStatus.UNAUTHORIZED.value(), response.statusCode());
    }

    @Test
    void crossTenantSessionIsNotFound() {
        var session = post("/api/ai/support/v1/sessions", "{\"channel\":\"web\"}", "other-key");
        assertEquals(HttpStatus.OK.value(), session.statusCode());
        var sessionId = JSON.readValue(session.body(), Map.class).get("id");

        var response = get("/api/ai/support/v1/sessions/" + sessionId, "demo-key");
        assertEquals(HttpStatus.NOT_FOUND.value(), response.statusCode());
    }

    @Test
    void writeToolRequiresApprovalAndIsIdempotent() {
        var context = new ToolExecutionContext("demo", "manager", Set.of("SUPPORT_MANAGER"),
                "session-1", "idem-refund-1", "trace-1");
        var pending = toolInvocations.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.PENDING_APPROVAL, pending.status());
        assertNotNull(pending.approvalId());

        var replay = toolInvocations.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.PENDING_APPROVAL, replay.status());

        var approved = toolInvocations.approve(pending.approvalId(), "manager", Set.of("SUPPORT_MANAGER"));
        assertEquals(ToolStatus.COMPLETED, approved.status());

        var completed = toolInvocations.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.COMPLETED, completed.status());
    }

    private HttpResponse<String> post(String path, String body, String apiKey) {
        var request = HttpRequest.newBuilder(uri(path))
                .header("Content-Type", "application/json")
                .header("X-API-Key", apiKey)
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build();
        return send(request);
    }

    private HttpResponse<String> get(String path, String apiKey) {
        var request = HttpRequest.newBuilder(uri(path))
                .header("X-API-Key", apiKey)
                .GET()
                .build();
        return send(request);
    }

    private URI uri(String path) {
        return URI.create("http://localhost:" + port + path);
    }

    private HttpResponse<String> send(HttpRequest request) {
        try {
            return http.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (Exception exception) {
            throw new IllegalStateException(exception);
        }
    }
}
