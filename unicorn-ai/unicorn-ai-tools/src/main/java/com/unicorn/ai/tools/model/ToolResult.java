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

package com.unicorn.ai.tools.model;

public record ToolResult(ToolStatus status, Object data, String message, String approvalId) {

    public static ToolResult completed(Object data) {
        return new ToolResult(ToolStatus.COMPLETED, data, "OK", null);
    }

    public static ToolResult pendingApproval(String approvalId, String message) {
        return new ToolResult(ToolStatus.PENDING_APPROVAL, null, message, approvalId);
    }

    public static ToolResult failed(String message) {
        return new ToolResult(ToolStatus.FAILED, null, message, null);
    }

    public boolean completed() {
        return status == ToolStatus.COMPLETED;
    }
}
