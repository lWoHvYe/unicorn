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

package com.unicorn.ai.tools.policy;

import com.unicorn.ai.tools.model.ApprovalStatus;
import com.unicorn.ai.tools.model.ToolExecutionContext;
import com.unicorn.ai.tools.model.ToolInvocation;
import com.unicorn.ai.tools.model.ToolPolicy;
import com.unicorn.ai.tools.model.ToolResult;
import com.unicorn.ai.tools.model.ToolStatus;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Single execution gate for read/write tools: role policy, idempotency, approval and audit.
 */
public class ToolInvocationService {

    private static final java.util.Set<String> APPROVER_ROLES = java.util.Set.of("SUPPORT_MANAGER", "TENANT_ADMIN");

    private final ToolInvocationRepository repository;
    private final Map<String, ToolPolicy> policies = new ConcurrentHashMap<>();
    private final Map<String, ToolExecutor> executors = new ConcurrentHashMap<>();

    public ToolInvocationService(ToolInvocationRepository repository) {
        this.repository = repository;
    }

    public void register(ToolPolicy policy, ToolExecutor executor) {
        policies.put(policy.name(), policy);
        executors.put(policy.name(), executor);
    }

    public ToolResult execute(ToolExecutionContext context, String toolName, Map<String, Object> args) {
        var policy = policies.get(toolName);
        if (policy == null) {
            return ToolResult.failed("未知工具: " + toolName);
        }
        if (policy.allowedRoles() != null && !policy.allowedRoles().isEmpty()
                && policy.allowedRoles().stream().noneMatch(context::hasRole)) {
            return ToolResult.failed("没有权限调用工具: " + toolName);
        }

        var safeArgs = args == null ? Map.<String, Object>of() : Map.copyOf(args);
        var idempotencyKey = context.idempotencyKey() == null || context.idempotencyKey().isBlank()
                ? hash(context.tenantId(), context.sessionId(), toolName, safeArgs)
                : context.idempotencyKey();

        var existing = repository.findByIdempotencyKey(context.tenantId(), idempotencyKey);
        if (existing.isPresent()) {
            return replay(existing.get());
        }

        var now = Instant.now();
        var invocation = new ToolInvocation(
                UUID.randomUUID().toString(), context.tenantId(), context.sessionId(), context.userId(),
                toolName, safeArgs, null, ToolStatus.FAILED, ApprovalStatus.NOT_REQUIRED,
                idempotencyKey, 0, context.traceId(), now, now);

        var amount = amount(safeArgs);
        if (policy.needsApproval(amount)) {
            var pending = invocation.withResult(ToolStatus.PENDING_APPROVAL, ApprovalStatus.PENDING, null, 0);
            repository.save(pending);
            return ToolResult.pendingApproval(pending.id(),
                    "该操作需要人工审批，审批单：" + pending.id());
        }

        return runAndSave(invocation, executors.get(toolName), safeArgs);
    }

    public ToolResult approve(String tenantId, String invocationId, String approverId,
                              java.util.Set<String> approverRoles) {
        if (approverRoles == null || approverRoles.stream().noneMatch(APPROVER_ROLES::contains)) {
            return ToolResult.failed("当前用户没有审批权限");
        }
        var invocation = repository.findById(tenantId, invocationId)
                .orElseThrow(() -> new java.util.NoSuchElementException("审批单不存在"));
        if (invocation.approvalStatus() != ApprovalStatus.PENDING) {
            return replay(invocation);
        }
        var approved = invocation.withApproval(ApprovalStatus.APPROVED);
        var result = runAndSave(approved, executors.get(invocation.toolName()), invocation.args());
        repository.saveApproval(invocationId, tenantId,
                result.status() == ToolStatus.COMPLETED ? "APPROVED" : "FAILED", approverId, null);
        return result;
    }

    public ToolResult reject(String tenantId, String invocationId, String approverId, String reason,
                             java.util.Set<String> approverRoles) {
        if (approverRoles == null || approverRoles.stream().noneMatch(APPROVER_ROLES::contains)) {
            return ToolResult.failed("当前用户没有审批权限");
        }
        var invocation = repository.findById(tenantId, invocationId)
                .orElseThrow(() -> new java.util.NoSuchElementException("审批单不存在"));
        if (invocation.approvalStatus() != ApprovalStatus.PENDING) {
            return replay(invocation);
        }
        var rejected = invocation.withResult(ToolStatus.REJECTED, ApprovalStatus.REJECTED,
                Map.of("reason", reason == null ? "" : reason), invocation.latencyMs());
        repository.save(rejected);
        repository.saveApproval(invocationId, tenantId, "REJECTED", approverId,
                reason == null ? "" : reason);
        return ToolResult.failed("审批已拒绝: " + (reason == null ? "" : reason));
    }

    public List<ToolInvocation> findPending(String tenantId) {
        return repository.findPending(tenantId);
    }

    private ToolResult runAndSave(ToolInvocation invocation, ToolExecutor executor, Map<String, Object> args) {
        var started = System.nanoTime();
        try {
            if (executor == null) {
                throw new IllegalStateException("工具未注册执行器: " + invocation.toolName());
            }
            var result = executor.execute(args);
            var latency = (System.nanoTime() - started) / 1_000_000;
            var completed = invocation.withResult(result.status(), approvalStatus(invocation),
                    result.data(), latency);
            repository.save(completed);
            return result;
        } catch (RuntimeException exception) {
            var latency = (System.nanoTime() - started) / 1_000_000;
            repository.save(invocation.withResult(ToolStatus.FAILED, approvalStatus(invocation),
                    Map.of("error", Objects.toString(exception.getMessage(), "tool failed")), latency));
            return ToolResult.failed(Objects.toString(exception.getMessage(), "工具执行失败"));
        }
    }

    private ApprovalStatus approvalStatus(ToolInvocation invocation) {
        return invocation.approvalStatus() == ApprovalStatus.APPROVED
                ? ApprovalStatus.APPROVED : ApprovalStatus.NOT_REQUIRED;
    }

    private ToolResult replay(ToolInvocation invocation) {
        if (invocation.status() == ToolStatus.PENDING_APPROVAL) {
            return ToolResult.pendingApproval(invocation.id(), "该操作仍在等待审批");
        }
        if (invocation.status() == ToolStatus.COMPLETED) {
            return ToolResult.completed(invocation.result());
        }
        if (invocation.status() == ToolStatus.REJECTED) {
            return ToolResult.failed("该操作已被拒绝");
        }
        return ToolResult.failed("该操作之前执行失败，请使用新的幂等键重试");
    }

    private BigDecimal amount(Map<String, Object> args) {
        var value = args.get("amount");
        if (value instanceof BigDecimal decimal) {
            return decimal;
        }
        if (value instanceof Number number) {
            return BigDecimal.valueOf(number.doubleValue());
        }
        if (value instanceof String text) {
            try {
                return new BigDecimal(text);
            } catch (NumberFormatException ignored) {
                return null;
            }
        }
        return null;
    }

    private String hash(String tenantId, String sessionId, String toolName, Map<String, Object> args) {
        try {
            var digest = MessageDigest.getInstance("SHA-256");
            var text = tenantId + "|" + Objects.toString(sessionId, "") + "|" + toolName + "|" + args;
            return HexFormat.of().formatHex(digest.digest(text.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException(exception);
        }
    }
}
