# unicorn-ai-support 设计与实现文档

> 文档版本：1.0  
> 最后更新：2026-10-30  
> 代码基线：Spring Boot 4.1.2、Spring AI 2.0.1、Java 25、Gradle 9.8.2  
> 适用范围：`unicorn-ai/unicorn-ai-support`、`unicorn-ai/unicorn-ai-tools`

## 1. 文档目的

本文记录 `unicorn-ai-support` 从“生产参考实现”推进到“PRD 生产基线”的设计、关键实现、数据模型、安全模型、运行方式、测试与已知边界。

配套文档：

- [PRD / SLO](prd/support-copilot.md)
- [模块 README](../README.md)
- [MCP 与 AI 模块说明](../../README.md)

本文描述的是**当前仓库真实代码**；未实现的能力会明确列在“已知边界与后续工作”，不会把规划写成已交付。

---

## 2. 背景与目标

### 2.1 业务场景

面向电商售后客服的多租户 copilot：

- 租户维护自己的知识库、订单、库存、商品数据。
- 客服/客户通过 REST 或 SSE 提问。
- 助手先检索知识库，再调用订单、退款、库存等工具。
- 高风险写操作受角色、金额阈值、幂等和人工审批保护。
- 低置信度或客户要求时转人工，生成工单。
- 外部 Agent 通过 MCP 复用同一套租户和工具策略。

### 2.2 目标

- 多租户隔离：应用层过滤 + PostgreSQL RLS。
- 可追溯：消息、工具、审批、文档、管理操作可审计。
- 可恢复：异步入库 outbox、失败清理、幂等、重试。
- 可控成本：模型超时/重试/降级、租户配额、限流。
- 可评测：citation 校验、retrieval recall/MRR 门禁。
- 可运维：Micrometer/Prometheus、K8s、CI、告警规则。

### 2.3 非目标

当前基线不包含：

- 完整管理端前端（只提供管理 API）。
- 多区域 active-active。
- 真实订单/库存服务的生产 adapter（当前是 JDBC demo adapter）。
- OCR、对象存储、病毒扫描的完整知识生产流水线。
- 全量 OpenTelemetry collector 与 Grafana 面板部署。

---

## 3. 总体架构

### 3.1 形态

保持**模块化单体**，暂不拆微服务：

```text
unicorn-ai-support (Spring Boot MVC application)
  ├── API / SSE / MCP
  ├── Conversation Orchestrator
  │    ├── ChatClient + Advisor Chain
  │    ├── JdbcChatMemoryRepository
  │    ├── RAG Retrieval + Citation Validation
  │    ├── Tool Policy / Approval / Audit
  │    ├── Model Invoker (timeout/retry/fallback)
  │    └── Quota / Rate Limit
  ├── Knowledge Ingestion + Outbox
  ├── Admin / IAM / Handoff / Eval
  └── PostgreSQL / pgvector / Redis / RabbitMQ

unicorn-ai-tools (plain library)
  ├── Order / Inventory / Product domain
  ├── Tool invocation idempotency
  ├── Approval state machine
  ├── Tool policy catalog
  └── Handoff repository abstraction
```

### 3.2 模块依赖

```text
unicorn-ai-support
  ├── depends on unicorn-ai-tools
  ├── Spring Boot Web MVC / Security / JDBC / AMQP / Redis / Actuator
  ├── Spring AI OpenAI / VectorStore / RAG / pgvector / MCP Server
  └── Flyway / PostgreSQL / H2 / springdoc

unicorn-ai-tools
  └── no Spring dependency; test-only JUnit
```

### 3.3 代码包地图

| 包 | 职责 |
| --- | --- |
| `api` | REST 控制器、异常处理 |
| `chat` | 会话、消息、ChatMemory、聊天编排、SSE |
| `retrieval` | 混合检索、RRF、MMR、rerank、citation、RAG retriever/augmenter |
| `knowledge` | 文档、版本、chunk、入库、生命周期 |
| `async` | RabbitMQ 异步入库、outbox |
| `tools` | 工具 facade、JDBC domain adapter、工具 invocation 配置 |
| `security` | API Key、JWT、租户上下文、RLS DataSource |
| `admin` | 租户用户/角色 |
| `handoff` | 人工工单查询/分配/关闭 |
| `audit` | 通用审计事件 |
| `quota` | 租户指标配额 |
| `ratelimit` | Redis/本地限流 |
| `eval` | retrieval 评测与门禁 |
| `mcp` | MCP tools/resources/prompts |
| `observability` | 指标 |
| `model` | 模型调用边界与 fallback |
| `config` | 配置、ChatClient、advisor、RLS、Flyway 等 |

---

## 4. 核心请求链路

### 4.1 认证与租户上下文

API Key 模式：

1. `ApiKeyAuthenticationFilter` 读取 `X-API-Key`。
2. `ApiKeyRepository.findActiveByHash` 查询启用且未过期的 key。
3. 构造 `AiPrincipal(tenantId, userId, roles)`。
4. 通过 `TenantContext.call(...)` 绑定 Java 25 `ScopedValue`。
5. 请求结束后 ScopedValue 自动结束，并清理 SecurityContext。

JWT 模式：

- `JwtDecoderConfig` 校验 issuer/audience（配置存在时）。
- `JwtSecurityConfig` 把 claims 映射为 `AiPrincipal`。
- 租户上下文从 SecurityContext/ScopedValue 获取。

关键点：

- 工具调用不再依赖 `ThreadLocal`；工具事件 sink 通过 `ToolContext` 显式传递。
- 缺少租户上下文时 fail-closed，不回退到 demo 租户。
- RLS 连接层在每次借出连接时设置 `app.tenant_id`；无租户时设置为空，策略失败关闭。

### 4.2 阻塞问答

入口：

```text
POST /api/ai/support/v1/sessions/{sessionId}/messages
```

流程：

```text
校验 message
  → RateLimiterService.checkChat
  → QuotaService.consumeChat
  → 校验 session 属于当前 tenant
  → 若带 Idempotency-Key，ai_chat_request 抢锁
  → 写 ai_chat_message(USER)
  → ChatClient + Advisors
       SafeGuardAdvisor
       MessageChatMemoryAdvisor
       RetrievalAugmentationAdvisor
         SupportDocumentRetriever (hybrid retrieval + ACL)
         SupportQueryAugmenter (citation prompt / empty-context refusal)
       ToolCalling (SupportToolFacade)
  → SupportModelInvoker (timeout/retry/optional fallback)
  → CitationValidator
  → 写 ai_chat_message(ASSISTANT)
  → usage 写 ai_usage_daily
  → finish ai_chat_request
```

`RetrievalCapture` 让 RAG advisor 的检索结果可以被业务层读取，用于返回 citations，而不是重复检索。

### 4.3 SSE 流式问答

入口：

```text
POST /api/ai/support/v1/sessions/{sessionId}/messages/stream
```

事件：

| event | 内容 |
| --- | --- |
| `meta` | messageId、traceId |
| `citation` | index、citation |
| `tool` | tool、status |
| `approval` | tool、approvalId、status |
| `token` | delta |
| `error` | code、message |
| `done` | messageId、status、usage |

实现要点：

- 先做一次检索用于 citation 预览，并写入 `RetrievalCapture`。
- advisor 的 retriever 检测到 capture 已有结果时直接复用，避免重复查询。
- `SseEmitter` 保存 `Disposable`，完成/超时/错误时取消上游。
- 流式 `chatResponse()` 聚合 usage，`done` 返回 usage。
- 引用校验失败时 `done.status=REFUSED`，持久化消息也是 `REFUSED`。
- 工具事件通过 `ToolContext` 中的 `eventSink` 发送，不依赖 ThreadLocal。

### 4.4 检索与 RAG

```text
QueryPlanService.plan
  → 每个 query:
       VectorStore.similaritySearch(tenant + kb filter)
       KnowledgeRepository.lexicalSearch(tenant + kb + ACTIVE)
  → RRF 融合
  → ACL 过滤
  → 可选 MMR
  → 可选 LLM rerank
  → Citation
```

ACL：

- `TENANT_ADMIN` 旁路。
- `SUPPORT_MANAGER` 继承 `SUPPORT_AGENT` 可见范围。
- 其他角色做精确匹配。
- 向量/词法候选先 overfetch，再做 ACL，减少私有文档挤占公开结果。

拒答：

- 检索结果为空时 `SupportQueryAugmenter` 抛 `NoContextException`，不调用模型。
- 模型回答中的 `[C1]` 由 `CitationValidator` 校验；越界/不存在引用改为 REFUSED。

### 4.5 工具与审批

工具由 `SupportToolFacade` 暴露为 Spring AI `@Tool`，统一经过 `ToolInvocationService`：

```text
tenant + session + tool + client idempotency key
  → 计算工具级 idempotency hash
  → tool policy: 角色、是否写操作、金额阈值
  → 只读工具直接执行
  → 写工具创建 approval: PENDING
  → 人工 approve:
        CAS PENDING -> APPROVING
        执行工具
        写 ai_approval / ai_tool_invocation
        -> COMPLETED / FAILED
```

写工具示例：

- `refund_order`：订单状态 `DELIVERED`，金额阈值触发审批。
- `reserve_inventory`：`available >= quantity` 条件更新。

事务：

- `SupportToolConfig` 把工具执行包装进 Spring `TransactionTemplate`。
- 订单/库存更新使用条件 SQL，检查 affected rows，避免多实例重复退款/超卖。

### 4.6 异步入库与 outbox

```text
上传文本/文件
  → DocumentIngestionService.ingest*
  → ai_document / ai_document_version / ai_ingest_job
  → OutboxRepository.enqueue(INGEST_REQUESTED)
  → 同一数据库事务提交
  → OutboxPublisher 定时 claim
  → RabbitMQ
  → AsyncIngestConsumer
       TenantContext.run(tenant)
       DocumentIngestionService.process
       → chunk → embedding → pgvector + ai_chunk
       → COMPLETED / FAILED / DEAD
```

可靠性：

- outbox 先落库，避免“消息已发但 DB 事务回滚”。
- 失败重试并写 `next_attempt_at`。
- 5 分钟后仍处于 `PUBLISHING` 的事件可被回收。
- 入库失败清理已写向量/chunk；chunkId 确定性生成，重复入库不会产生孤儿。

### 4.7 MCP

- `/mcp` 使用同一 API Key/JWT 与租户上下文。
- `SupportMcpTools`、`SupportMcpResources`、`SupportMcpPrompts` 复用工具/知识策略。
- MCP 工具纳入限流。
- 旧 `unicorn-ai-mcp-server` 仍是单租户 demo；生产以 support 的 `/mcp` 为准。

---

## 5. 数据模型

### 5.1 迁移

| 迁移 | 内容 |
| --- | --- |
| `common/V1` | 租户、API Key、知识库、文档、版本、chunk、job、会话、消息、反馈、工具、审批、usage |
| `postgres/V2` | pgvector / pg_trgm |
| `common/V3` | 订单、库存、商品 demo 表 |
| `common/V4` | 唯一索引、chat request 幂等表 |
| `common/V5` | IAM、配额、outbox、审计、prompt/model、评测、工单、知识审核 |
| `common/V6` | Spring AI ChatMemory |
| `postgres/V7` | Row Level Security |

### 5.2 主要表

| 表 | 作用 |
| --- | --- |
| `ai_tenant` / `ai_api_key` | 租户与认证 |
| `ai_tenant_user` / `ai_role` / `ai_user_role` | 租户用户/角色 |
| `ai_knowledge_base` / `ai_document` / `ai_document_version` / `ai_chunk` | 知识库 |
| `ai_ingest_job` | 入库任务 |
| `ai_outbox` | 异步入库 outbox |
| `ai_chat_session` / `ai_chat_message` / `ai_chat_request` | 会话/消息/聊天幂等 |
| `ai_chat_memory` | Spring AI ChatMemory |
| `ai_feedback` | 用户反馈 |
| `ai_tool_invocation` / `ai_approval` | 工具调用与审批 |
| `ai_usage_daily` | token/成本聚合 |
| `ai_handoff_ticket` | 人工工单 |
| `ai_audit_event` | 管理与生命周期审计 |
| `ai_quota` | 租户配额 |
| `ai_prompt_version` / `ai_model_route` | 提示词/模型治理（表已建，管理面待完善） |
| `ai_eval_*` | 评测数据集/运行（表已建，持久化 API 待完善） |
| `ai_knowledge_review` | 知识审核（表已建） |
| `ai_order` / `ai_inventory` / `ai_product` | local demo domain |

### 5.3 关键约束

- `ai_api_key.key_hash` 唯一。
- `ai_tool_invocation(tenant_id, idempotency_key)` 唯一。
- `ai_chat_request(tenant_id, session_id, request_id)` 唯一。
- `ai_document_version(document_id, version_no)` 唯一。
- `ai_chunk(document_id, version_id, chunk_index)` 唯一。
- `ai_outbox(status, next_attempt_at)` 索引。
- `ai_audit_event(tenant_id, created_at)` 索引。
- `ai_handoff_ticket(tenant_id, status, updated_at)` 索引。

---

## 6. 安全模型

### 6.1 多层租户隔离

1. **认证层**：API Key/JWT 解析出 `AiPrincipal`。
2. **应用层**：Repository SQL 显式带 `tenant_id`。
3. **上下文层**：`ScopedValue` 防止异步/工具调用时租户漂移。
4. **数据库层**：PostgreSQL RLS `FORCE ROW LEVEL SECURITY`。
5. **连接层**：`TenantAwareDataSource` 设置 `app.tenant_id`；无上下文时为空，RLS fail-closed。

RLS 覆盖直接带 `tenant_id` 的业务表；以下表是**有意排除**的系统/无直接租户列表：

- `ai_api_key`：认证前查找。
- `ai_outbox`：系统调度器跨租户 claim。
- `ai_chat_memory`：通过 `tenant:session` conversationId 隔离。
- `ai_document_version` / `ai_eval_case` / `ai_user_role`：无直接 tenant 列，由上层校验。

生产数据库账号必须是非 owner、非 superuser；否则 PostgreSQL 会绕过 RLS。

### 6.2 API Key 生命周期

- 创建：`POST /admin/api-keys`，只返回一次明文。
- 吊销：`POST /admin/api-keys/{id}/revoke`。
- 缓存：`ai-api-key` 缓存，吊销时主动 evict。
- 审计：`ai_audit_event` 记录创建/吊销。

### 6.3 授权

- `TENANT_ADMIN`：租户、用户、角色、配额、API Key、知识管理。
- `SUPPORT_MANAGER`：知识发布/退役、审批、工单。
- `SUPPORT_AGENT`：会话、只读工具、发起审批/转人工。
- 写工具受金额阈值与人工审批约束。

### 6.4 限流、配额与模型治理

- `RateLimiterService`：Redis/本地限流。
- `QuotaService`：`ai_quota` 的 `chat/DAY` 配额；无配额行时不限制。
- `SupportModelInvoker`：超时、有限重试、可选 fallback。
- `SafeGuardAdvisor`：敏感词拦截。
- `CitationValidator`：引用越界/伪造检测。

### 6.5 审计

- 工具/审批：`ai_tool_invocation` + `ai_approval`。
- 管理操作/工单/知识状态：`ai_audit_event`。
- 所有关键请求带 `traceId`，异常响应不泄露堆栈。

---

## 7. Spring AI 2 编排

### 7.1 Advisor Chain

阻塞问答：

```java
chatClient.prompt()
    .advisors(safeGuardAdvisor, memoryAdvisor, ragAdvisor)
    .user(message)
    .tools(toolFacade)
    .toolContext(...)
    .call()
    .chatResponse();
```

流式使用同一组 advisor，但先预检索并写入 `RetrievalCapture`，让 `SupportDocumentRetriever` 复用结果。

### 7.2 ChatMemory

- `JdbcChatMemoryRepository implements ChatMemoryRepository`。
- `MessageWindowChatMemory` 最大 20 条。
- conversationId 为 `tenantId:sessionId`。
- 存储表 `ai_chat_memory`；`ai_chat_message` 继续作为业务消息/审计。

### 7.3 RAG

- `SupportDocumentRetriever implements DocumentRetriever`：调用现有 `RetrievalService`，从 `Query.context()` 读取 tenant/kb/roles/capture。
- `SupportQueryAugmenter implements QueryAugmenter`：生成 `[C1]` 格式上下文；空上下文抛 `NoContextException`。
- `RetrievalAugmentationAdvisor` 使用独立 `ThreadPoolTaskExecutor` 与 `ContextPropagatingTaskDecorator`。

### 7.4 模型调用

`SupportModelInvoker`：

- 虚拟线程执行。
- `timeoutMs`、`maxAttempts`。
- 可选 `fallback-model`，主模型失败后切换 fallback ChatClient。
- Micrometer `Observation("ai.support.model")`。
- `NoContextException` 不重试，直接用于拒答。

---

## 8. Java 25 实践

- `ScopedValue`：租户上下文，替代 ThreadLocal。
- 虚拟线程：模型调用边界、Spring MVC/异步执行。
- `record` / pattern matching / sequenced collection：DTO、领域记录、列表处理。
- `StructuredTaskScope` 仍是 preview；当前主路径没有把它作为唯一执行方式，避免生产运行时依赖 preview 语义。
- 不同线程的上下文通过显式参数（`AiPrincipal`、`ToolContext`、`RetrievalCapture`）传递，不依赖隐式线程本地状态。

---

## 9. 可观测性

### 9.1 指标

| 指标 | 说明 |
| --- | --- |
| `ai.support.chat` | 聊天耗时/状态 |
| `ai.support.retrieval` | 检索耗时/是否为空 |
| `ai.support.tool.calls` | 工具调用/状态 |
| `ai.support.ingest.jobs` | 入库任务状态 |
| `ai.support.citation.invalid` | citation 校验失败 |
| `ai.support.quota.exceeded` | 配额耗尽 |
| `ai.support.model` | 模型调用 observation |

### 9.2 告警

`k8s/monitoring/prometheus-rules.yaml` 提供：

- chat 错误率 > 5%
- retrieval empty rate > 30%
- citation invalid
- quota exceeded

`k8s/monitoring/README.md` 列出建议 dashboard 维度。

### 9.3 日志

- 不打印 prompt/answer/tool 参数全文。
- 关键异常只返回固定文案 + traceId。
- 审计事件写数据库，不依赖日志。

---

## 10. 部署与配置

### 10.1 Profile

| profile | 存储 | 模型 | 依赖 |
| --- | --- | --- | --- |
| 默认 `local` | H2 + SimpleVectorStore | Demo 模型 | 零外部依赖 |
| `infra` | PostgreSQL/pgvector + Redis + RabbitMQ | OpenAI-compatible | 真实基础设施 |

### 10.2 关键配置

```yaml
ai:
  support:
    vector-store: pgvector
    rls:
      enabled: true
    async-ingest:
      enabled: true
    model:
      timeout-ms: 30000
      max-attempts: 2
      fallback-model: ""
    eval:
      min-recall-at-5: 0.80
      min-mrr: 0.60
```

### 10.3 Docker / K8s

- Dockerfile：Java 25 JRE，非 root 用户。
- `compose.yaml`：PG/pgvector、Redis、RabbitMQ、support。
- `k8s/`：Deployment、Service、Ingress、HPA、PDB、Secret 示例。
- health：`/actuator/health/liveness`、`/actuator/health/readiness`。

### 10.4 CI

`.github/workflows/ai-support.yml`：

- 单元/集成测试
- `unicorn-ai-support:assemble` / `unicorn-ai-tools:assemble`
- Docker build

---

## 11. API 概览

Base：`/api/ai/support/v1`

| 方法 | 路径 | 说明 |
| --- | --- | --- |
| POST | `/sessions` | 创建会话 |
| POST | `/sessions/{id}/messages` | 阻塞问答 |
| POST | `/sessions/{id}/messages/stream` | SSE |
| GET | `/sessions/{id}/messages` | 历史 |
| POST | `/sessions/{id}/feedback` | 反馈 |
| POST | `/sessions/{id}/close` | 关闭并摘要 |
| GET | `/handoffs` | 工单列表 |
| POST | `/handoffs/{id}/assign` | 分配 |
| POST | `/handoffs/{id}/close` | 关闭 |
| POST | `/knowledge-bases/{kb}/documents/text` | 文本入库 |
| POST | `/documents/{id}/status` | REVIEW/ACTIVE/RETIRED |
| POST | `/documents/{id}/reindex` | 重建索引 |
| DELETE | `/documents/{id}` | 删除 |
| GET | `/jobs/{id}` | 入库状态 |
| GET/POST | `/approvals` | 审批 |
| POST | `/admin/users` `/admin/roles` `/admin/quotas` `/admin/api-keys` | 管理 |
| POST | `/eval/run` | 检索评测 |
| `/mcp` | MCP Streamable HTTP | MCP |

完整请求示例见 [README](../README.md)。

---

## 12. 测试与验证

### 12.1 测试分层

- 单元：工具策略/幂等/审批、citation、eval、JWT。
- 本地集成：H2 + SimpleVectorStore，`SupportApplicationTests`。
- Testcontainers：PG/pgvector + RabbitMQ，`SupportInfraIntegrationTest`（无 Docker 时 skip）。
- 真实外部 infra：PG/pgvector + Redis + RabbitMQ，`SupportExternalInfraTest`（`AI_INFRA_TEST=true`）。
- RLS：外部测试创建非 owner 角色，验证 tenant 隔离。
- outbox：外部测试断言 `PUBLISHED`。

### 12.2 命令

```bash
./gradlew :unicorn-ai-tools:test :unicorn-ai-support:test
AI_INFRA_TEST=true ./gradlew :unicorn-ai-support:test \
  --tests com.unicorn.ai.support.SupportExternalInfraTest
./gradlew assemble
```

### 12.3 当前结果

- `unicorn-ai-tools:test`：6 个通过。
- `unicorn-ai-support:test`：21 个通过；Testcontainers/外部测试默认 skip。
- 外部 PG/Redis/RabbitMQ：2 个通过（异步入库 + RLS）。
- `./gradlew assemble`：BUILD SUCCESSFUL。

---

## 13. 设计决策与取舍

| 决策 | 原因 | 代价 |
| --- | --- | --- |
| 模块化单体 | 首期交付速度、事务简单 | 未来需按 worker/检索/工具拆分 |
| 应用过滤 + RLS | 防单点遗漏 | DB 账号与连接设置要求更高 |
| 自研混合检索 | 需要 ACL、RRF、MMR、rerank 组合 | 需要自己维护评测 |
| Spring AI Advisor | 复用 ChatMemory/RAG/Safety/observation | 需要控制 advisor 顺序 |
| outbox | 异步入库可靠 | 增加调度与幂等复杂度 |
| 人工审批 | 高风险写操作可控 | 需要审批 UI/通知 |
| JDBC demo domain | 本地可运行、测试稳定 | 生产需替换真实服务 adapter |

---

## 14. 已知边界与后续工作

### 14.1 未完成

- OpenTelemetry 全链路 tracing、Grafana 面板、SLO error budget。
- 对象存储、OCR/表格解析、病毒扫描、知识审核发布 UI。
- 真实订单/库存/商品服务 adapter、契约测试、Webhook。
- golden eval 数据集持久化、CI 评测任务、A/B prompt/model。
- `ai_prompt_version` / `ai_model_route` / `ai_eval_*` 的完整管理 API。
- 负载测试、安全测试、混沌测试、备份恢复/DR 演练。
- MCP legacy server 仍为单租户 demo。

### 14.2 风险

- RLS 依赖正确的 DB 账号和连接设置；owner/superuser 会绕过。
- `StructuredTaskScope` 为 preview，不应作为唯一生产路径。
- Outbox 的 claim 是 at-least-once，消费端仍需依赖确定性 ID/幂等。
- SSE + 虚拟线程 + Reactor 的取消/背压需要持续压测。
- 自研检索需要持续维护 golden set，不能只靠线上反馈。

---

## 15. 维护指南

- 修改迁移时同步更新本文的数据模型和 RLS 表清单。
- 修改 API 时同步更新 API 表和 README。
- 新增 advisor 时明确顺序并补测试。
- 新增租户表时同步加 RLS 策略或在本文说明排除原因。
- 新增工具时同步更新 `SupportToolCatalog`、审批策略和审计。
- 发布前至少执行：

```bash
./gradlew assemble
./gradlew :unicorn-ai-tools:test :unicorn-ai-support:test
```

涉及真实基础设施时再跑 `AI_INFRA_TEST=true` 外部测试。

---

## 16. 相关提交

本设计对应的关键提交（从生产基线到当前）：

```text
e8be7ddb feat(ai): expose citation and quota telemetry
4e9d2455 feat(ai): add knowledge review and retirement lifecycle
724ca75b feat(ai): enforce tenant daily chat quotas
74e7885d feat(ai): add optional fallback model routing
588dd5c3 feat(ai): add API key rotation and revocation
6ff2bb04 feat(ai): gate eval quality and recover stale outbox claims
aaf84149 feat(ai): enforce PostgreSQL row-level tenant isolation
f95ea308 chore(ai): add support CI and Kubernetes assets
a05112b0 feat(ai): publish ingestion through durable outbox
9bf47689 feat(ai): validate citations and report stream usage
```

更早的 tool/RAG/chat 基础提交见 `git log -- unicorn-ai/unicorn-ai-support`。
