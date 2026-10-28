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

package com.unicorn.ai.support.chat;

import java.time.Instant;

public final class ChatRecords {

    private ChatRecords() {
    }

    public record Session(String id, String tenantId, String userId, String channel, String kbId, String title,
                          String status, String summary, Instant createdAt, Instant updatedAt) {
    }

    public record Message(String id, String sessionId, String tenantId, String role, String content,
                          String citationsJson, String toolCallsJson, String usageJson, String traceId,
                          String status, Instant createdAt) {
    }

    public record Feedback(String id, String sessionId, String messageId, String tenantId,
                           int rating, String comment) {
    }

    public record Request(String tenantId, String sessionId, String requestId, String messageId, String status) {
    }
}
