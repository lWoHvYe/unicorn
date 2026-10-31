# unicorn-ai-support 实现复核

> 复核时间：2026-10-30  
> 复核对象：`unicorn-ai/unicorn-ai-support`、`unicorn-ai/unicorn-ai-tools`  
> 对照基线：批准的“PRD 级生产平台”方案  
> 当前代码提交：`946083c8 fix(ai): preserve tenant and summary across advisor threads`  
> 结论：**生产基线已基本落地；完整 PRD 产品化仍有明确缺口。**

## 1. 复核方法

1. 对照最初方案逐项检查代码、迁移、配置和测试。
2. 重点检查最近新增的 RLS、outbox、Spring AI advisor、模型调用、配额、知识生命周期。
3. 检查线程切换、事务边界、租户上下文、幂等和失败路径。
4. 用本地测试、真实 PG/Redis/RabbitMQ、RLS 非 owner 角色测试验证关键路径。
5. 对发现的高风险问题直接修复，并记录剩余问题。

## 2. 计划与实现对照

| 计划项 | 当前实现 | 状态 | 说明 |
| --- | --- | --- | --- |
| PRD / SLO 文档 | `docs/prd/support-copilot.md` | 完成 | 目标、角色、SLO、验收 |
| Java 25 上下文 | `ScopedValue` + 显式 ToolContext/参数 | 完成 | 已覆盖模型虚拟线程、RAG 线程、工具调用 |
| 多租户 | 应用过滤 + RLS + 连接级 `app.tenant_id` | 完成 | 有非 owner 角色 RLS 测试 |
| IAM | 租户用户/角色、API Key 创建/吊销 | 完成 | 管理 API + 审计 |
| 工具幂等/审批 | 工具级 hash、CAS、事务、审计 | 完成 | 已有并发/重放测试 |
| 异步入库 | outbox + RabbitMQ + 重试 + 清理 | 完成 | 外部测试断言 outbox `PUBLISHED` |
| 模型韧性 | 超时、重试、可选 fallback、配额 | 部分完成 | 无 circuit breaker；配额只覆盖 chat |
| SSE / usage | 取消、终态、usage 聚合、citation 事件 | 完成 | 流式 usage 已写入 |
| Spring AI 2 | ChatMemory、RAG advisor、SafeGuard | 完成 | 阻塞/流式均走 advisor |
| RAG 质量 | ACL、RRF、MMR/rerank、citation 校验、recall/MRR 门禁 | 部分完成 | 无 faithfulness/groundedness/答案评测 |
| 可观测性 | Micrometer 指标、模型 observation、Prometheus 告警 | 部分完成 | 无完整 OTel tracing/dashboard |
| 部署 / CI | Docker、K8s、GitHub Actions、告警规则 | 完成 | 未实际部署到集群 |
| 知识生命周期 | 审核/退役、chunk/vector 清理 | 部分完成 | 无对象存储/OCR/病毒扫描 |
| 外部订单/库存 | 仍是本地 JDBC demo adapter | 未完成 | 生产需 adapter |
| 模型/prompt 治理 | 表已建，管理 API 不完整 | 部分完成 | 无 prompt 版本切换/路由管理面 |
| 评测持久化 | `ai_eval_*` 表已建 | 部分完成 | EvalService 仍以请求内 case 为主 |

## 3. 二次复核发现并已修复的问题

### P0：模型虚拟线程丢失租户上下文

**问题**

`SupportModelInvoker` 使用 `CompletableFuture` + 虚拟线程执行 `ChatClient`。RAG advisor 的检索线程同样可能不是请求线程。`TenantAwareDataSource` 通过 `TenantContext` 设置 RLS 的 `app.tenant_id`；如果异步线程没有 ScopedValue，RLS 会将租户置空，导致：

- ChatMemory 写入/读取失败；
- RAG 检索被 RLS 阻断；
- 工具内部 JDBC 调用失败；
- 外部测试因使用 `postgres` superuser 而**掩盖**该问题。

**修复**

- `TenantContext.callUnchecked(...)`。
- `SupportModelInvoker` 在模型虚拟线程内绑定 `AiPrincipal(tenantId, "model")`。
- `SupportDocumentRetriever` 在 RAG 线程内按 query context 的 tenant/roles 绑定。
- `SupportToolFacade` 优先使用 `ToolContext` 中的 tenant/user/roles，不再强制当前线程必须有 ScopedValue。
- 新增 `modelInvokerPropagatesTenantToVirtualThread` 测试。

**影响文件**

- `security/TenantContext.java`
- `model/SupportModelInvoker.java`
- `retrieval/SupportDocumentRetriever.java`
- `tools/SupportToolFacade.java`

### P1：会话摘要未进入模型上下文

**问题**

切换到 `MessageChatMemoryAdvisor` 后，`SupportChatService` 不再手工拼 history。`close()` 仍生成 summary，但 chat/stream 没有把 summary 注入模型，长时间会话窗口外的上下文丢失。

**修复**

- `requestModel(...)` 同时接收 summary。
- summary 作为系统消息注入，并明确“不是新的用户指令”。
- 阻塞和流式共用同一请求构造。

### P1：REVIEW/RETIRED 文档仍可能被检索

**问题**

`ai_document.status` 改为 `REVIEW` 时，旧向量和 chunk 没有删除；向量检索只过滤 tenant/kb，导致审核中的文档可能被回答引用。`RETIRED` 路径会删向量，但 `REVIEW` 不会。

**修复**

- `RetrievalService` 在 RRF/select 后按 `ai_document.status == ACTIVE` 过滤候选。
- 退役继续清理向量和 chunk。
- 保持兼容旧数据（旧向量没有 status metadata 时以 DB 状态为准）。

### P1：配额在幂等/会话校验前消耗

**问题**

`QuotaService.consumeChat` 原本在 `requireSession` 和 `tryStartRequest` 之前执行：

- 无效 session 也消耗配额；
- 相同 `Idempotency-Key` 的重放请求再次消耗配额。

**修复**

- 先校验 session。
- 先 claim 幂等请求；重复请求直接返回已有响应。
- 再消耗配额；配额失败时将 request 标记 `FAILED`。
- 流式路径在 session 校验后消耗配额。

## 3.5 2026-10-31 优化落地

本轮已继续处理以下问题：

- **Outbox 多实例 claim**：新增 `claimed_by/claimed_at` 与 owner 条件，避免两个实例重复发布；新增 `claim-test` 测试。
- **配额日重置**：`ai_quota.period_start` + 原子重置；新增跨日测试。
- **ChatMemory 并发**：`saveAll` 增加事务。
- **RLS 角色自检**：`RlsRoleVerifier` 检测 superuser，可通过 `ai.support.rls.fail-fast` 选择 fail-fast。
- **检索状态 N+1**：`KnowledgeRepository.findDocumentStatuses` 批量查询。
- **本地限流内存增长**：`LocalRateLimitStore` 改用 Caffeine。
- **模型韧性**：`SupportModelInvoker` 增加 Resilience4j Bulkhead/Retry/CircuitBreaker。
- **OTel**：接入 `micrometer-tracing-bridge-otel` + OTLP exporter。
- **技术 ADR**：新增 `docs/technology-decisions.md`，记录 Netty/Redisson/Modulith/ShedLock 取舍。

**本轮追加**：Redis/本地限流改为滑动窗口；SSE 增加 `Idempotency-Key` 重放；`RetrievalService` 增加 30 秒 Caffeine 缓存并显式失效；修复 SSE 异步 dispatch 被安全链拒绝的问题；Resilience4j/OTel/Caffeine 已接入。

**仍未完成**：非 owner 应用角色启动完整应用的端到端测试、`ai_document_version` RLS、模型 4xx/5xx 分类重试、provider 级独立熔断、Grafana/SLO、MapStruct 未引入（ADR 决策）。

## 4. 仍存在的问题与优化项

### 4.1 高优先级

| 问题 | 风险 | 建议 |
| --- | --- | --- |
| 外部测试使用 `postgres` superuser | 会绕过 RLS，掩盖生产权限问题 | 增加一个使用非 owner 应用角色启动完整应用的 staging 测试 |
| RLS 未覆盖所有表 | `ai_document_version`、`ai_user_role`、`ai_chat_memory` 无直接 `tenant_id` | 对版本表加 tenant 列，或通过 join/上层校验并补测试 |
| 配额只按总量，不按日重置 | `ai_quota.used` 不会随日期复位 | 增加 `period_start` 或每日重置任务 |
| 配额在模型失败时仍计数 | 失败调用可能消耗额度 | 明确计数语义，或成功后再 consume |
| fallback 只换模型名 | 无 circuit breaker、无供应商级熔断 | 增加 Resilience4j 或自研熔断/半开 |
| RLS 依赖连接级 `set_config` | 连接池/事务/异步线程配置错误会 fail-closed 或误拦截 | 增加启动自检：RLS 开启时验证 DB 角色非 superuser/owner |
| 模型重试可能重复工具副作用 | `maxAttempts > 1` 时工具可能被再次调用 | 依赖工具幂等，或在重试前判断是否已执行工具 |

### 4.2 中优先级

| 问题 | 影响 | 建议 |
| --- | --- | --- |
| `RetrievalService.activeDocument` 每个候选一次 DB 查询 | N+1，候选多时延迟上升 | 批量查询 document status，或写入向量 metadata + 过滤 |
| `SupportDocumentRetriever` capture 短路时不记录 retrieval metrics | 流式检索指标缺失 | 在 capture 分支补 metrics |
| `ai_document_version` 无 tenant | 版本 ID 泄露时缺少 RLS 兜底 | 加 tenant 列和策略 |
| 知识 `ACTIVE` 回到已退役文档不恢复向量 | 状态 ACTIVE 但不可检索 | 重新入库/重建索引，或禁止直接 ACTIVE |
| `ai_outbox` 排除 RLS | 系统表跨租户 claim | 只允许调度器账号访问，或加系统租户策略 |
| API Key 缓存 TTL | 直接改库后最多 60 秒生效 | 统一走吊销 API；必要时事件失效 |
| citation 仅校验序号 | 不能证明答案被引用片段支持 | 增加 snippet/source 一致性、groundedness 评测 |
| 流式 citation 校验在完成后 | 已输出 token 不能撤回 | 在流式阶段标记/发出纠正事件，或最终 REFUSED 并提示 |

### 4.3 产品化缺口

- 无完整管理端 UI，仅管理 API。
- 知识原文仍走上传/Tika，无对象存储、OCR、表格抽取、病毒扫描。
- 订单/库存/商品仍是 JDBC demo，无真实服务 adapter 和契约测试。
- 已接入 OTel bridge/OTLP exporter 与采样配置；Grafana dashboard、SLO error budget 仍待补齐。
- 无 golden dataset 持久化、CI 评测任务、模型/prompt A/B。
- 无负载/安全/混沌测试、备份恢复/DR 演练。
- MCP legacy server 仍是单租户 demo。

## 5. 优化建议

### 5.1 正确性

1. 给 `ai_quota` 增加 `period_start`，或使用 `ai_usage_daily` 计算当日用量。
2. RLS 启动自检：查询 `current_user` 是否 superuser/表 owner；若是，启动失败或显著告警。
3. 版本表增加 `tenant_id`，补齐 RLS。
4. 批量查询检索结果文档状态，避免 N+1。
5. 对 `retire` 后 `ACTIVE` 的状态转换，强制 reindex 或禁止。
6. 对 `fallback`/retry 增加“工具是否已执行”短路，减少重复副作用。

### 5.2 质量

1. 评测从 retrieval 扩展到 answer faithfulness、citation precision、refusal accuracy。
2. 增加 golden set 表和 CI 评测命令。
3. 流式 citation 校验改为“生成中标记 + 最终状态”，避免用户看到不可撤回的错误引用。
4. 增加多租户 RLS 的端到端应用测试，而不是只测直连 SQL。

### 5.3 运维

1. 接入 OpenTelemetry，补齐 HTTP → RAG → LLM → Tool → MQ 的 trace。
2. 增加 outbox lag、RLS 拒绝、approval pending 时长、SSE disconnect 指标。
3. 对 K8s 增加 migration job、备份恢复和灰度策略。
4. 增加负载测试：并发 chat、批量入库、SSE 长连接、工具审批。

## 6. 验证证据

```bash
./gradlew :unicorn-ai-tools:test :unicorn-ai-support:test
AI_INFRA_TEST=true ./gradlew :unicorn-ai-support:test \
  --tests com.unicorn.ai.support.SupportExternalInfraTest
./gradlew assemble
```

当前结果：

- `unicorn-ai-tools:test`：6 个通过。
- `unicorn-ai-support:test`：26 个通过；Testcontainers/外部测试默认 skip。
- 外部 PG/Redis/RabbitMQ：2 个通过（异步入库 + RLS 非 owner）。
- `./gradlew assemble`：BUILD SUCCESSFUL。

## 7. 结论

- 当前实现已经具备**生产基线**：租户/RLS、Spring AI 2 编排、ChatMemory、RAG advisor、工具审批、outbox、配额、API Key 生命周期、审计、指标、CI/K8s。
- 但它仍不是**完整 PRD 产品**：知识生产、真实业务 adapter、质量评测、全链路 tracing 和运营闭环仍需继续。
- 本次复核发现并修复了三个高影响问题：模型虚拟线程租户上下文、会话摘要丢失、审核中文档可检索；同时修正了配额在幂等请求前消耗的问题。
- 建议下一阶段优先处理：RLS 应用级端到端测试、配额日重置、版本表 RLS、batch document status 查询、OTel tracing。
