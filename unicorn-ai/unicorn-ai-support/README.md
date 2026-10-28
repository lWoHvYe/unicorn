# unicorn-ai-support

面向中等规模生产场景的 **多租户售后客服智能体** 参考实现。它不是另一个 demo，而是把 Chat、RAG、工具调用、审批、MCP、审计和指标串成一条可运行的业务链路。

## 场景

- 租户（商家）维护自己的知识库与订单/库存数据。
- 客服或客户通过 REST/SSE 提问；助手先检索知识库，再调用订单、退款、库存工具。
- 退款/库存预留等写操作受角色、金额阈值、幂等和人工审批保护。
- 外部 Agent 通过 `/mcp` 使用同一套工具策略与审计。
- 会话、知识库、文档 ACL、工具审计按 `tenant_id` 隔离；所有工具调用可追溯。

## 模块

| 模块 | 作用 |
| --- | --- |
| `unicorn-ai-support` | 生产应用：会话、RAG、工具、审批、MCP、SSE、指标 |
| `unicorn-ai-tools` | 订单/退款/库存领域、工具策略、幂等、审批、审计原语 |

现有 `unicorn-ai-chat` / `rag` / `mcp-*` 仍保留为最小示例；生产参考以本模块为准。

## 本地启动

默认 `local` profile 零外部依赖：H2 + SimpleVectorStore + 本地演示模型，不需要 API Key。

```bash
./gradlew :unicorn-ai-support:bootRun
# 端口 8096
```

API Key（仅本地演示）：

```text
X-API-Key: demo-key        # tenant=demo
X-API-Key: other-key       # tenant=other，用于隔离测试
```

## Infra 启动

使用现有 PostgreSQL/pgvector、Redis、RabbitMQ：

```bash
export AI_DB_URL='jdbc:postgresql://10.211.55.29:5434/vectordb?currentSchema=ai_support'
export AI_DB_USER=postgres
export AI_DB_PWD=postgres
export REDIS_HOST=10.211.55.29
export REDIS_PASSWORD=redis-psd
export MQ_HOST=10.211.55.29
export MQ_USER=admin
export MQ_PWD=admin

./gradlew :unicorn-ai-support:bootRun --args='--spring.profiles.active=infra'
```

`infra` 会：
- 执行 `db/migration/common` + `db/migration/postgres`（pgvector/pg_trgm）；
- 使用 PostgreSQL 元数据/chunk + PgVectorStore + `ai_order`/`ai_inventory`/`ai_product`；
- 打开 RabbitMQ 异步入库；
- 默认关闭本地演示模型（需配置 `OPENAI_API_KEY` / `OPENAI_BASE_URL`）；`AI_DEMO_MODEL=true` 可继续使用确定性演示模型。

### Docker

```bash
# 在仓库根目录构建并启动支持应用 + pgvector + Redis + RabbitMQ
cd unicorn-ai/unicorn-ai-support
docker compose up --build
```

Compose 会设置 `AI_DEMO_SEED=true`、`AI_DEMO_MODEL=true`，默认使用 `X-API-Key: demo-key`。生产部署请覆盖数据库/Redis/MQ 密码和模型 Key。

## 核心 API

Base：`/api/ai/support/v1`

| 方法 | 路径 | 说明 |
| --- | --- | --- |
| POST | `/sessions` | 创建会话，可带 `kbId` |
| POST | `/sessions/{id}/messages` | 阻塞问答，返回 citations/usage |
| POST | `/sessions/{id}/messages/stream` | SSE：`meta/token/citation/tool/approval/done/error` |
| GET | `/sessions/{id}/messages` | 历史消息 |
| POST | `/sessions/{id}/feedback` | 反馈 |
| POST | `/sessions/{id}/close` | 关闭 |
| GET/POST | `/knowledge-bases` | 知识库列表/创建 |
| POST | `/knowledge-bases/{kbId}/documents` | 上传文档（multipart） |
| POST | `/knowledge-bases/{kbId}/documents/text` | 文本入库 |
| GET | `/knowledge-bases/{kbId}/documents` | 文档列表 |
| POST | `/documents/{id}/reindex` | 重建索引 |
| DELETE | `/documents/{id}` | 删除 |
| GET | `/jobs/{id}`、`/jobs` | 入库任务 |
| GET | `/approvals` | 待审批工具调用 |
| POST | `/approvals/{id}/approve`、`/reject` | 审批 |
| MCP | `/mcp` | Streamable-HTTP，共用 API Key/JWT |

示例：

```bash
# 入库
curl -s localhost:8096/api/ai/support/v1/knowledge-bases/demo-kb/documents/text \
  -H 'Content-Type: application/json' -H 'X-API-Key: demo-key' \
  -d '{"text":"Spring Boot 4.1 是当前版本。退款需在签收后 7 天内申请。","source":"faq.md","title":"FAQ","docType":"faq"}'

# 会话 + 问答
SESSION=$(curl -s localhost:8096/api/ai/support/v1/sessions \
  -H 'Content-Type: application/json' -H 'X-API-Key: demo-key' \
  -d '{"channel":"web","title":"demo"}' | jq -r .id)

curl -s localhost:8096/api/ai/support/v1/sessions/$SESSION/messages \
  -H 'Content-Type: application/json' -H 'X-API-Key: demo-key' \
  -d '{"message":"退款需要几天内申请？"}'
```

SSE：

```bash
curl -N -X POST localhost:8096/api/ai/support/v1/sessions/$SESSION/messages/stream \
  -H 'Content-Type: application/json' -H 'X-API-Key: demo-key' \
  -d '{"message":"Spring Boot 4.1"}'
```

## 混合检索

- 向量：`VectorStore.similaritySearch`，过滤 `tenant/kbId`；再按文档 `aclRoles` 过滤。
- 词法：`ai_chunk` 的 `LOWER(content) LIKE`；Postgres 额外建 `pg_trgm` GIN 索引。
- RRF：`1/(60+rank)` 合并；默认 MMR 去冗余，`rerank.enabled=true` 时再走 LLM 重排。
- 无结果/低于阈值直接拒答，不调用生成模型。
- `query.rewrite-enabled=true` 时先做 query rewrite + multi-query。
- 入库时可用 `aclRoles`（逗号分隔）限制文档可见角色；为空表示租户内公开。
- Citation 包含 `documentId/versionId/chunkId/source/title/snippet/score`。

## 工具与审批

只读：`get_order`、`refund_eligibility`、`search_products`、`check_inventory`、`create_handoff`。

写操作：`refund_order`、`reserve_inventory`。

`ToolInvocationService` 统一处理：
- 角色校验；
- `Idempotency-Key`（或 session/tool/args hash）唯一约束；
- 金额超过 `ai.support.approval.refund-threshold` 进入 `PENDING_APPROVAL`；
- 审批后执行（`SUPPORT_MANAGER` / `TENANT_ADMIN`）；
- `ai_tool_invocation` 审计参数、结果、耗时、traceId；`ai_approval` 记录审批状态/审批人/原因。

## MCP

`/mcp` 与 REST 共用 API Key/JWT 过滤器。工具：
`get_order`、`refund_eligibility`、`refund_order`、`check_inventory`、`reserve_inventory`、`create_handoff`。
Resources：`unicorn://orders/{orderId}`、`unicorn://inventory/{sku}`。
Prompts：`order-support`（按租户订单生成客服回复草稿）。
外部 Agent 只能看到本租户数据；写工具同样走策略/审批/审计。

## 可观测性

- `/actuator/health`、`/actuator/prometheus`（Prometheus 端点同样受 API Key/JWT 保护，抓取端需带 `X-API-Key` 或 Bearer token）。
- `ai.support.chat` timer、`ai.support.ingest.jobs`、`ai.support.tool.calls`（Chat 与 MCP 工具）。
- `ai_chat_message.trace_id`、`ai_tool_invocation.trace_id`。
- `ai_usage_daily` 记录 token/请求数和成本；`ai.support.pricing.input-per-1k-tokens` / `output-per-1k-tokens` 配置单价（默认 0），按每次调用的 token 计算。
- 日志默认不打印用户原文和工具完整参数。

## 测试

```bash
./gradlew :unicorn-ai-tools:test
./gradlew :unicorn-ai-support:test
```

测试使用 `local` profile、H2、SimpleVectorStore 和确定性演示模型，不需要真实模型 Key；覆盖：
- 入库→检索→citation；
- 401、跨租户 404；
- 写工具审批/幂等。

## 已知边界

- 默认使用 H2 + 进程内向量库，重启丢数据；生产用 `infra`。
- 中文词法检索用 `LIKE`/`pg_trgm` 近似；接口保留 `LexicalRetriever`，可换 OpenSearch/专用分词。
- 本地演示模型只用于跑通链路，不用于质量评估。
- 检索默认启用 MMR；`query.rewrite-enabled` 和 `rerank.enabled` 为可选开关，默认关闭以控制成本。
- `infra` 限流/API Key 缓存使用 Redis；Redis 故障时限流 fail-closed，缓存需要 Redis 可用（生产应配置高可用）。
- JWT 需显式设置 `ai.security.mode=jwt` 并提供 `JWT_ISSUER_URI` / `JWT_JWK_SET_URI` / `JWT_HMAC_SECRET` 之一。
- MCP 旧模块 `unicorn-ai-mcp-server` 已加 API Key（默认 `demo-key`），但不具备租户隔离；生产 MCP 以本模块 `/mcp` 为准。
- MCP client 支持 `DEMO_AI_MCP_CLIENT_API_KEY` 和 `DEMO_AI_MCP_CLIENT_ALLOWLIST`。
- 领域数据使用 `ai_order`/`ai_inventory`/`ai_product` JDBC 表；本地 demo 由 `DemoDataInitializer` 写入，生产可替换为外部订单/库存服务适配器。
- `SupportInfraIntegrationTest` 使用 Testcontainers 启动 PostgreSQL/pgvector + RabbitMQ，验证异步入库、job 状态和 citation；无 Docker 时自动 skip。
- Docker 资产：`unicorn-ai/unicorn-ai-support/Dockerfile`、`compose.yaml`。
- 完整生产化还需要：对象存储、评测集与 CI 门禁、真实 Redis/生产数据库 HA 联调。
