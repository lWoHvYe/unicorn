# Unicorn AI Demo 模块说明

## 模块划分

`unicorn-ai` 目录下包含四个独立应用：

- `unicorn-ai-chat`：ChatClient、流式响应、对话记忆、结构化输出、工具调用与 Agent 编排，端口 8092。
- `unicorn-ai-rag`：文档 ETL、切分、向量检索、显式 RAG、Advisor RAG、查询改写与多查询扩展，端口 8093。
- `unicorn-ai-mcp-server`：通过 MCP 暴露订单、库存工具以及资源与提示词，端口 8094。
- `unicorn-ai-mcp-client`：连接 MCP Server，把远程工具注入 ChatClient，端口 8095。

## 运行顺序

MCP 场景先启动 `unicorn-ai-mcp-server`，再以 `MCP_CLIENT_ENABLED=true` 启动 `unicorn-ai-mcp-client`。RAG 场景先配置 `OPENAI_API_KEY`，再调用 `/api/ai/rag/ingest/sample` 导入示例文档。

## 环境变量

- `OPENAI_API_KEY`：OpenAI 或兼容服务的 API Key。
- `OPENAI_BASE_URL`：兼容服务地址，例如 DeepSeek、vLLM、Ollama 的 OpenAI 兼容端点。
- `OPENAI_CHAT_MODEL`：对话模型名。
- `OPENAI_EMBEDDING_MODEL`：向量模型名。
- `RAG_VECTOR_STORE`：`simple` 或 `redis`。
- `REDIS_HOST`、`REDIS_PORT`、`REDIS_PASSWORD`：Redis Stack 连接信息。
- `MCP_SERVER_URL`：MCP Server 地址，默认 `http://localhost:8094`。

## 安全边界

MCP 的 HTTP 端点默认没有认证，能访问端口的客户端可以枚举并调用全部工具。Demo 绑定 localhost 仅用于本地演示；部署到其他环境前必须在网关或 Spring Security 层增加认证、授权与限流。
