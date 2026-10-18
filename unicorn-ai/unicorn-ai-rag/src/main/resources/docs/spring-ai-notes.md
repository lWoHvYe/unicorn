# Spring AI 2.0 速记

## 版本与兼容性

Spring AI 2.0.x 支持 Spring Boot 4.0.x 与 4.1.x，制品发布在 Maven Central，使用 `org.springframework.ai:spring-ai-bom` 统一管理版本。Spring AI 2.0 的 OpenAI 实现基于官方 `openai-java` SDK，流式调用仍然依赖 Reactor。

## ChatClient

`ChatClient` 是同步与流式对话的统一入口。同步使用 `call()`，流式使用 `stream()`；`content()` 返回字符串，`chatResponse()` 返回带 token 用量与 finish reason 的完整响应，`entity()` 把结果映射为 Java record。

## Advisors

Advisor 负责在请求前后增强 Prompt 或处理响应。常用 advisor 包括：

- `MessageChatMemoryAdvisor`：把历史消息注入 Prompt，并把本轮消息写回 `ChatMemory`。
- `QuestionAnswerAdvisor`：基于 `VectorStore` 做简单 RAG。
- `RetrievalAugmentationAdvisor`：模块化 RAG，把 query 转换、检索、上下文增强组合起来。
- `SimpleLoggerAdvisor`：以 DEBUG 级别记录请求与响应。
- `ToolCallingAdvisor`：Spring AI 2.0 自动注册，负责工具调用循环。

`ChatMemory.CONVERSATION_ID` 必须通过 `.advisors(a -> a.param(...))` 显式传入，否则运行期抛 `IllegalArgumentException`。

## Tool Calling

在方法上标注 `@Tool`，参数用 `@ToolParam` 描述，然后通过 `ChatClient.prompt().tools(bean)` 注册。Spring AI 2.0 会发送工具定义、接收 tool call、执行工具、把结果回填给模型，直到模型给出最终答复。

`ToolContext` 参数不会进入 JSON Schema，可用于传递 tenant、userId、traceId 等非模型上下文。`@Tool(returnDirect = true)` 可让工具结果直接返回调用方，不再经过模型二次总结。

## MCP

MCP（Model Context Protocol）把工具、资源、提示词标准化。Spring AI 同时提供客户端与服务端 starter：

- 服务端：`spring-ai-starter-mcp-server-webmvc`，通过 `@McpTool`、`@McpResource`、`@McpPrompt` 暴露能力。
- 客户端：`spring-ai-starter-mcp-client`，自动发现 MCP 工具并暴露为 `SyncMcpToolCallbackProvider`。
- 传输：STDIO、SSE（2.0 起弃用）、Streamable-HTTP、Stateless Streamable-HTTP。

## RAG 与 ETL

ETL 管线由 `DocumentReader`、`DocumentTransformer`、`DocumentWriter` 组成。常见组合是 `TikaDocumentReader` 或 `MarkdownDocumentReader` 读取文档，`TokenTextSplitter` 切块，`VectorStore` 写入向量库。

`TokenTextSplitter` 默认使用 CL100K_BASE 编码，chunk 默认 800 token。中文内容建议显式配置中文标点作为断句点，避免长段落被硬切。

## 可观测性

Spring AI 为 chat、embedding、tool、vector store 发出 Micrometer observations。引入 actuator 与 tracing bridge 后，可以在日志/链路系统中看到 `gen_ai.client.operation`、`spring.ai.tool` 等 observation。
