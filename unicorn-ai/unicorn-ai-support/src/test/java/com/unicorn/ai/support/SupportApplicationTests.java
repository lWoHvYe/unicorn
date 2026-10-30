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
import org.springframework.jdbc.core.JdbcTemplate;
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

    @Autowired
    private JdbcTemplate jdbc;

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

        var closed = post("/api/ai/support/v1/sessions/" + sessionId + "/close", "{}", "demo-key");
        assertEquals(HttpStatus.OK.value(), closed.statusCode());
        var sessionAfter = get("/api/ai/support/v1/sessions/" + sessionId, "demo-key");
        var sessionBody = JSON.readValue(sessionAfter.body(), Map.class);
        assertNotNull(sessionBody.get("summary"));
        assertEquals("CLOSED", sessionBody.get("status"));
    }

    @Test
    void restrictedDocumentIsNotRetrievedByAgent() {
        var ingest = post("/api/ai/support/v1/knowledge-bases/demo-kb/documents/text", """
                {"text":"acl-secret-2026 只允许客服经理查看。",
                 "source":"acl/restricted.md","title":"Restricted","docType":"faq",
                 "aclRoles":"SUPPORT_MANAGER"}
                """, "demo-key");
        assertEquals(HttpStatus.OK.value(), ingest.statusCode());

        var session = post("/api/ai/support/v1/sessions", "{\"channel\":\"web\"}", "agent-key");
        assertEquals(HttpStatus.OK.value(), session.statusCode());
        var sessionId = JSON.readValue(session.body(), Map.class).get("id");

        var chat = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"acl-secret-2026\"}", "agent-key");
        assertEquals(HttpStatus.OK.value(), chat.statusCode());
        var body = JSON.readValue(chat.body(), Map.class);
        assertEquals("REFUSED", body.get("status"));
        assertTrue(((java.util.List<?>) body.get("citations")).isEmpty());
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
    void chatRequestIdempotencyReturnsSameAssistantMessage() {
        var session = post("/api/ai/support/v1/sessions", "{\"channel\":\"web\"}", "demo-key");
        var sessionId = JSON.readValue(session.body(), Map.class).get("id");
        var first = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"Spring Boot 4.1\"}", "demo-key", "chat-idem-1");
        var second = post("/api/ai/support/v1/sessions/" + sessionId + "/messages",
                "{\"message\":\"Spring Boot 4.1\"}", "demo-key", "chat-idem-1");
        assertEquals(HttpStatus.OK.value(), first.statusCode());
        assertEquals(HttpStatus.OK.value(), second.statusCode());
        var firstBody = JSON.readValue(first.body(), Map.class);
        var secondBody = JSON.readValue(second.body(), Map.class);
        assertEquals(firstBody.get("messageId"), secondBody.get("messageId"));
        assertEquals(1, jdbc.queryForObject(
                "SELECT COUNT(*) FROM ai_chat_message WHERE session_id = ? AND role = 'USER'",
                Integer.class, sessionId));
    }

    @Test
    void tenantAdminCreatesUserRoleAndAssignsIt() {
        var user = post("/api/ai/support/v1/admin/users",
                "{\"externalSubject\":\"user-admin-test\",\"displayName\":\"Admin Test\",\"email\":\"a@example.com\"}",
                "demo-key");
        assertEquals(HttpStatus.OK.value(), user.statusCode());
        var userId = JSON.readValue(user.body(), Map.class).get("id");

        var role = post("/api/ai/support/v1/admin/roles",
                "{\"name\":\"QA_REVIEWER\",\"scopes\":\"knowledge:read\"}", "demo-key");
        assertEquals(HttpStatus.OK.value(), role.statusCode());
        var roleId = JSON.readValue(role.body(), Map.class).get("id");

        var assigned = post("/api/ai/support/v1/admin/users/" + userId + "/roles/" + roleId, "{}", "demo-key");
        assertEquals(HttpStatus.OK.value(), assigned.statusCode());
        assertTrue(assigned.body().contains("QA_REVIEWER"));

        var forbidden = get("/api/ai/support/v1/admin/users", "agent-key");
        assertEquals(HttpStatus.FORBIDDEN.value(), forbidden.statusCode());
    }

    @Test
    void handoffIsPersistedAndTenantScoped() {
        var context = new ToolExecutionContext("demo", "manager", Set.of("SUPPORT_MANAGER"),
                "session-handoff", "handoff-1", "trace-handoff");
        var result = toolInvocations.execute(context, "create_handoff",
                Map.of("tenantId", "demo", "sessionId", "session-handoff", "userId", "manager",
                        "reason", "需要人工客服"));
        assertEquals(ToolStatus.COMPLETED, result.status());
        var ticketId = ((com.unicorn.ai.tools.model.HandoffTicket) result.data()).id();
        assertNotNull(ticketId);

        var list = get("/api/ai/support/v1/handoffs", "demo-key");
        assertEquals(HttpStatus.OK.value(), list.statusCode());
        assertTrue(list.body().contains(ticketId));

        var assigned = post("/api/ai/support/v1/handoffs/" + ticketId + "/assign",
                "{\"assigneeId\":\"agent-1\"}", "demo-key");
        assertEquals(HttpStatus.OK.value(), assigned.statusCode());
        assertTrue(assigned.body().contains("ASSIGNED"));

        var closed = post("/api/ai/support/v1/handoffs/" + ticketId + "/close", "{}", "demo-key");
        assertEquals(HttpStatus.OK.value(), closed.statusCode());
        assertTrue(closed.body().contains("CLOSED"));
    }

    @Test
    void anotherTenantCannotApproveThroughHttp() {
        var context = new ToolExecutionContext("demo", "manager", Set.of("SUPPORT_MANAGER"),
                "session-cross", "cross-tenant-approval-1", "trace-cross");
        var pending = toolInvocations.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.PENDING_APPROVAL, pending.status());

        var response = post("/api/ai/support/v1/approvals/" + pending.approvalId() + "/approve", "{}", "other-key");
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

        var approved = toolInvocations.approve("demo", pending.approvalId(), "manager", Set.of("SUPPORT_MANAGER"));
        assertEquals(ToolStatus.COMPLETED, approved.status());
        assertEquals(1, jdbc.queryForObject(
                "SELECT COUNT(*) FROM ai_approval WHERE id = ? AND status = 'APPROVED'",
                Integer.class, pending.approvalId()));

        var completed = toolInvocations.execute(context, "refund_order",
                Map.of("tenantId", "demo", "orderId", "A1004", "amount", new BigDecimal("899.00")));
        assertEquals(ToolStatus.COMPLETED, completed.status());
    }

    private HttpResponse<String> post(String path, String body, String apiKey) {
        return post(path, body, apiKey, null);
    }

    private HttpResponse<String> post(String path, String body, String apiKey, String idempotencyKey) {
        var builder = HttpRequest.newBuilder(uri(path))
                .header("Content-Type", "application/json")
                .header("X-API-Key", apiKey);
        if (idempotencyKey != null) {
            builder.header("Idempotency-Key", idempotencyKey);
        }
        return send(builder.POST(HttpRequest.BodyPublishers.ofString(body)).build());
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
