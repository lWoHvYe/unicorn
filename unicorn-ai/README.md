# unicorn-ai

Spring AI **2.0.1** 的工程集合，运行在 Spring Boot **4.1.x** / Java **25** 上。其中 `unicorn-ai-support` 是面向中等规模生产场景的多租户客服智能体参考实现；其余子目录保留为最小 demo。

> Spring AI 2.0.x 官方支持 Spring Boot 4.0.x / 4.1.x；制品在 Maven Central，BOM 为 `org.springframework.ai:spring-ai-bom:2.0.1`。

## 模块与端口

| 目录 | Gradle 工程 | 端口 | 内容 |
| --- | --- | --- | --- |
| `unicorn-ai-chat` | `:unicorn-ai-chat` | 8092 | ChatClient、SSE 流式、对话记忆、结构化输出、`@Tool` 工具调用、Agent 两步编排 |
| `unicorn-ai-rag` | `:unicorn-ai-rag` | 8093 | ETL/切分、Simple/pgvector/Redis Stack 向量库、显式 RAG、Advisor RAG、Query Rewrite、Multi-Query、RabbitMQ 异步入库 |
| `unicorn-ai-mcp-server` | `:unicorn-ai-mcp-server` | 8094 | `@McpTool` / `@McpResource` / `@McpPrompt`，Streamable-HTTP |
| `unicorn-ai-mcp-client` | `:unicorn-ai-mcp-client` | 8095 | 通过 MCP 发现工具并注入 ChatClient |
| `unicorn-ai-support` | `:unicorn-ai-support` | 8096 | **生产参考**：多租户知识库、混合检索、citations、会话/SSE、工具策略/审批/审计、MCP |
| `unicorn-ai-tools` | `:unicorn-ai-tools` | - | support 的领域/策略/审批库 |

这些 build 文件使用了非默认名（如 `unicorn-ai-chat.gradle.kts`），所以工程路径被拍平到根，**不是** `:unicorn-ai:unicorn-ai-chat`。

`unicorn-ai-support` 的完整场景、API、数据模型、测试和生产边界见 [unicorn-ai-support/README.md](unicorn-ai-support/README.md)。它默认使用 H2 + 本地演示模型，`infra` profile 使用 PostgreSQL/pgvector + Redis + RabbitMQ。

## 快速开始

```bash
export OPENAI_API_KEY=sk-...
# 可选：兼容 OpenAI 的服务
# export OPENAI_BASE_URL=https://api.deepseek.com
# export OPENAI_CHAT_MODEL=deepseek-chat
# export OPENAI_EMBEDDING_MODEL=text-embedding-3-small

./gradlew :unicorn-ai-chat:bootRun
./gradlew :unicorn-ai-rag:bootRun
./gradlew :unicorn-ai-mcp-server:bootRun

# 生产参考：本地无需 key，H2 + SimpleVectorStore + demo model
./gradlew :unicorn-ai-support:bootRun
```

没有 API Key 时应用也能启动：`api-key` 使用占位值，真正调用模型/embedding 时才会失败。RAG 的启动导入默认关闭，避免启动阶段调用 embedding。

MCP 客户端默认关闭。先启动 server，再：

```bash
MCP_API_KEY=demo-key ./gradlew :unicorn-ai-mcp-server:bootRun & MCP_CLIENT_ENABLED=true MCP_API_KEY=demo-key ./gradlew :unicorn-ai-mcp-client:bootRun
```

所有应用都暴露 actuator：`/actuator/health`、`/actuator/metrics`。Spring AI 的 chat/embedding/tool 调用会产生 Micrometer observations，接入 tracing 后可在链路系统中查看。

## 复用 Unicorn 现有基础设施（infra profile）

默认 profile 是零依赖 demo（`SimpleVectorStore` + 内存记忆 + 内存任务表）。如果要连接现有 dev 环境，使用 `infra` profile：

```bash
# Chat：Redis 持久化对话记忆
./gradlew :unicorn-ai-chat:bootRun --args='--spring.profiles.active=infra'

# RAG：pgvector 向量库 + RabbitMQ 异步入库 + MySQL 任务审计
./gradlew :unicorn-ai-rag:bootRun --args='--spring.profiles.active=infra'
```

连接信息与 `valentine-starter/src/main/resources/config/application-dev.yml`、`document/docker/docker-compose-env-sim.yml` 对齐：

| 服务 | 地址 | 认证 | 用途 |
| --- | --- | --- | --- |
| Redis Stack | `10.211.55.29:6379` | `redis-psd` | Chat 对话记忆（db 6）+ RAG 向量库（索引 `unicorn-ai-rag`，db 0） |
| pgvector | `10.211.55.29:5434/vectordb` | `postgres/postgres` | 备选 RAG 向量库（表 `unicorn_ai_rag`） |
| MySQL | `10.211.55.29:3306/unicorn` | `root/root` | RAG 异步任务审计（表 `ai_rag_ingest_job`） |
| RabbitMQ | `10.211.55.29:5672`，vhost `/unicorn` | `admin/admin` | RAG 异步入库队列 |

说明：

- Redis Stack 已加载 **search + ReJSON** 模块，RAG infra 默认使用 Redis 向量库；索引 `unicorn-ai-rag` 的维度由 `EMBEDDING_DIMENSIONS` 控制（默认 1536）。
- 启动时不会调用 embedding 探测维度：Redis 分支使用了固定维度的 `EmbeddingModel` 包装，只有真正入库/检索时才请求模型。
- 如果 Redis 换成普通镜像（无 RediSearch），可切换 pgvector：`--demo.ai.rag.vector-store=pgvector`；否则启动会报 `ERR unknown command 'FT._LIST'`。
- `infra` 启动时会主动连接这些服务；默认 profile 不会。
- compose sim 的 RabbitMQ 默认是 `guest/guest`，如果未创建 `admin/admin`，用 `MQ_USER=guest MQ_PWD=guest` 覆盖。
- MySQL 需要先有 `unicorn` 库（可导入 `sql/unicorn.sql`）；`ai_rag_ingest_job` 表由 `schema.sql` 自动创建。

## 测试

AI demo 模块单独配置了 `useJUnitPlatform()`，以下测试不依赖模型 API Key：

```bash
./gradlew :unicorn-ai-chat:test
./gradlew :unicorn-ai-rag:test
./gradlew :unicorn-ai-mcp-server:test
./gradlew :unicorn-ai-tools:test
./gradlew :unicorn-ai-support:test
```

覆盖内容：工具调用的租户上下文/退款规则/商品搜索、MCP 订单与库存领域逻辑、RAG 文本切分与 metadata 注入。

## Chat Demo（8092）

```bash
# 普通对话
curl -s localhost:8092/api/ai/chat \
  -H 'Content-Type: application/json' \
  -d '{"message":"用三句话解释什么是 RAG","conversationId":"c1"}'

# 流式（SSE）
curl -N 'localhost:8092/api/ai/chat/stream?message=写一首关于秋天的短诗&conversationId=c1'

# 查看对话记忆（窗口默认 20 条）
curl -s localhost:8092/api/ai/chat/memory/c1

# 工具调用：订单查询/退款判断/商品搜索
curl -s localhost:8092/api/ai/chat/tools \
  -H 'Content-Type: application/json' \
  -d '{"message":"帮我查 A1001 的状态，并判断能不能退款","conversationId":"tools-1"}'

# A1004 是签收 1 天内的订单，可用于验证“可退款”分支
curl -s localhost:8092/api/ai/chat/tools \
  -H 'Content-Type: application/json' \
  -d '{"message":"A1004 能退款吗？","conversationId":"tools-2"}'

# 结构化输出（默认走 prompt 约束）
curl -s localhost:8092/api/ai/chat/structured \
  -H 'Content-Type: application/json' \
  -d '{"topic":"Spring AI 2.0 在企业的落地","providerNative":false}'

# 结构化输出 + provider 原生 JSON Schema + 本地校验
curl -s localhost:8092/api/ai/chat/structured \
  -H 'Content-Type: application/json' \
  -d '{"topic":"Spring AI 2.0 在企业的落地","providerNative":true}'

# 两步 Agent：先规划，再带工具执行
curl -s localhost:8092/api/ai/chat/agent \
  -H 'Content-Type: application/json' \
  -d '{"message":"A1002 能不能退款？如果不能，推荐一个库存充足的替代品"}'
```

实现要点：

- `MessageChatMemoryAdvisor` + `MessageWindowChatMemory` 负责对话记忆；每次调用都必须传 `ChatMemory.CONVERSATION_ID`。
- 记忆 advisor 默认位于工具循环之外：只持久化最终 user/assistant 消息，不保存中间 tool call；需要完整工具轨迹时可把 advisor order 调到 `ToolCallingAdvisor` 之后。
- 工具通过 `@Tool` / `@ToolParam` 声明，`ToolContext` 传递 tenant 等非模型上下文；`@Tool(returnDirect = true)` 演示“工具结果直接返回”。
- `spring.ai.tools.limits.max-total-tool-calls=8` 限制单轮工具调用总数，防止循环失控。
- `SimpleLoggerAdvisor` 需要把 `org.springframework.ai.chat.client.advisor` 调到 DEBUG 才能看到请求/响应。
- `infra` profile 下 `MessageWindowChatMemory` 使用 `RedisChatMemoryRepository`，对话可跨重启恢复；key 前缀和 TTL 见上表。

## RAG Demo（8093）

```bash
# 导入 classpath:docs/*.md（需要 embedding key）
curl -s -X POST localhost:8093/api/ai/rag/ingest/sample

# 上传文件：md/txt/pdf/docx 等
curl -s -X POST localhost:8093/api/ai/rag/documents \
  -F 'file=@README.md' -F 'tenant=demo' -F 'docType=markdown'

# 直接导入一段文本
curl -s localhost:8093/api/ai/rag/documents/text \
  -H 'Content-Type: application/json' \
  -d '{"text":"Unicorn 的默认端口是 8081。","source":"inline/port","tenant":"demo"}'

# 只检索，不调用 chat
curl -s 'localhost:8093/api/ai/rag/search?q=Spring%20AI%20支持哪些版本&topK=3'

# 显式 RAG：返回 answer + sources + token usage
curl -s localhost:8093/api/ai/rag/ask \
  -H 'Content-Type: application/json' \
  -d '{"question":"Spring AI 2.0 支持哪些 Spring Boot 版本？","topK":3}'

# Advisor RAG：由 RetrievalAugmentationAdvisor 完成检索与上下文增强
curl -s localhost:8093/api/ai/rag/ask/advisor \
  -H 'Content-Type: application/json' \
  -d '{"question":"MCP 的 HTTP 端点默认有认证吗？","filter":"tenant == '\''sample'\''"}'

# Query Rewrite / Multi-Query
curl -s localhost:8093/api/ai/rag/query/rewrite \
  -H 'Content-Type: application/json' -d '{"question":"它支持 4.1 吗"}'

curl -s localhost:8093/api/ai/rag/query/expand \
  -H 'Content-Type: application/json' -d '{"question":"怎么提高 RAG 召回率"}'

# 查看/删除某个来源
curl -s localhost:8093/api/ai/rag/stats
curl -s -X DELETE 'localhost:8093/api/ai/rag/documents?source=sample/faq.md'

# infra profile：异步入库（RabbitMQ），立即返回 jobId
curl -s -X POST localhost:8093/api/ai/rag/documents/text/async \
  -H 'Content-Type: application/json' \
  -d '{"text":"异步入库的内容","source":"infra/demo","tenant":"demo","docType":"text"}'

# 查询任务状态（MySQL）
curl -s localhost:8093/api/ai/rag/jobs/<jobId>
```

向量库：

- 默认 `demo.ai.rag.vector-store=simple`，使用进程内 `SimpleVectorStore`，零外部依赖，重启丢数据。
- `infra` profile 使用 Redis Stack：索引 `unicorn-ai-rag`，HNSW + COSINE，维度 1536；启动时用固定维度包装避免调用 embedding。
- 备选 pgvector：`--demo.ai.rag.vector-store=pgvector`，连接 `jdbc:postgresql://10.211.55.29:5434/vectordb`，自动创建 `unicorn_ai_rag` 表。
- 文档 metadata 会补齐 `source`、`tenant`、`docType`、`ingestedAt`、`contentHash`，可用于 `filter` 表达式。
- 切分使用 `TokenTextSplitter`，中文标点参与断句；chunk 大小、阈值、topK 都在 `application.yml` 中可调。

## MCP Demo

Server（8094）暴露：

- Tools：`get_order`、`refund_eligibility`、`refund_order`、`check_inventory`、`reserve_inventory`
- Resources：`unicorn://orders/{orderId}`、`unicorn://inventory/{sku}`、`unicorn://runbook/deploy`
- Prompt：`order-support`

Client（8095）启动后：

```bash
curl -s localhost:8095/api/ai/mcp/status
curl -s localhost:8095/api/ai/mcp/tools
curl -s localhost:8095/api/ai/mcp/chat \
  -H 'Content-Type: application/json' \
  -d '{"message":"查一下 A1001 的状态，并看看 SKU-1 还有多少库存"}'
```

MCP 端点默认是 `/mcp`（Streamable-HTTP）。`unicorn-ai-mcp-server` 现在要求 `X-API-Key`（需显式设置 `MCP_API_KEY`），但仍是单租户 demo；生产多租户 MCP 使用 `unicorn-ai-support` 的 `/mcp`。客户端可通过 `DEMO_AI_MCP_CLIENT_API_KEY` 和 `DEMO_AI_MCP_CLIENT_ALLOWLIST` 配置认证与工具白名单。

## 切换模型 / Provider

OpenAI 官方：

```bash
OPENAI_API_KEY=sk-...
OPENAI_BASE_URL=https://api.openai.com
OPENAI_CHAT_MODEL=gpt-4o-mini
OPENAI_EMBEDDING_MODEL=text-embedding-3-small
```

DeepSeek（注意它主要提供 chat，不一定提供 embedding）：

```bash
OPENAI_API_KEY=sk-...
OPENAI_BASE_URL=https://api.deepseek.com
OPENAI_CHAT_MODEL=deepseek-chat
```

Ollama（OpenAI 兼容端点）：

```bash
OPENAI_BASE_URL=http://localhost:11434/v1
OPENAI_CHAT_MODEL=llama3.1
OPENAI_EMBEDDING_MODEL=nomic-embed-text
OPENAI_API_KEY=ollama
```

不同模型对 tool calling、JSON Schema structured output 的支持不同。`/structured` 的 `providerNative=true` 需要 provider 支持 JSON Schema；不支持时用 `false` 走 prompt 约束 + 本地转换。

## 设计取舍

- **Demo 与生产参考分离**：`unicorn-ai-chat`/`rag`/`mcp-*` 仍是最小 demo；生产参考看 `unicorn-ai-support`（API Key/JWT、多租户、文档版本、citations、审批、审计、指标）。
- **显式 RAG vs Advisor RAG**：显式 RAG 便于返回 citations、分数、trace；Advisor RAG 代码更少，但来源等中间结果需要从 advisor context 里取。Demo 同时保留两者。
- **结构化输出双路径**：prompt 约束兼容性最好；provider 原生 schema 更可靠，但依赖模型能力。
- **模板安全**：用户输入与检索到的文档通过 `{message}` / `{prompt}` 参数传入，避免内容里的 `{}` 被 StringTemplate 当成占位符（既防报错，也减少 prompt 注入面）。
- **MCP 与本地工具同构**：两者最终都是 `ToolCallback`，可以混用；Demo 拆成 server/client 两个应用，避免同进程启动时序问题。
- **可观测性**：actuator + Micrometer observations，Spring AI 会自动为 chat、embedding、tool、vector store 生成 observation。

## 常见问题

- **401 / 模型不存在**：检查 `OPENAI_API_KEY`、`OPENAI_BASE_URL`、`OPENAI_CHAT_MODEL`；兼容服务的 base URL 通常需要带 `/v1`。
- **MCP client 启动失败**：确认 server 已启动，或保持 `MCP_CLIENT_ENABLED=false`。客户端启动时会初始化连接。
- **RAG 启动失败**：不要开启 `demo.ai.rag.ingest-on-startup`，除非 embedding key 与额度已就绪。
- **Redis 向量检索失败（`ERR unknown command 'FT._LIST'`）**：说明当前 Redis 不是 Redis Stack；用 `infra` profile 的默认 Redis 向量库需要 `redis/redis-stack` 镜像，或改用 `--demo.ai.rag.vector-store=pgvector`。
- **`infra` 启动失败**：依次检查 Redis 密码、pgvector 的 5434 端口、MySQL `unicorn` 库是否存在、RabbitMQ 的 `admin/admin` 与 `/unicorn` vhost；compose sim 默认 RabbitMQ 是 `guest/guest`，可用 `MQ_USER` / `MQ_PWD` 覆盖。
- **异步接口返回 503**：说明 `async-ingest` 未启用；用 `--spring.profiles.active=infra`，默认 profile 只支持同步入库。
- **工具没有被调用**：模型需支持 tool calling；`@Tool` 描述要明确；参数类型要可 JSON Schema 化（不要用 `Optional`/`Mono`/`Flux`）。
