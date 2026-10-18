# 常见问题

## 没有 API Key 能不能启动？

可以启动。Demo 使用 `demo-key-not-configured` 作为占位值，只有真正调用模型或 embedding 时才会失败。RAG 的 `ingest-on-startup` 默认关闭，避免启动时调用 embedding。

## 如何切换到 DeepSeek？

设置：

- `OPENAI_BASE_URL=https://api.deepseek.com`
- `OPENAI_CHAT_MODEL=deepseek-chat`
- `OPENAI_API_KEY=<your key>`

如果该服务不提供 embedding，RAG 模块需要单独配置一个 embedding 端点，或把 `SearchRequest` 改成基于关键词的检索。

## 如何切换到本地 Ollama？

Ollama 提供 OpenAI 兼容端点：

- `OPENAI_BASE_URL=http://localhost:11434/v1`
- `OPENAI_CHAT_MODEL=llama3.1`
- `OPENAI_EMBEDDING_MODEL=nomic-embed-text`

注意：不同模型的工具调用能力不同，部分本地模型不支持 tool calling。

## 为什么工具没有被调用？

检查模型是否支持 tool calling；检查 `@Tool` 描述是否清晰；检查 `.tools(...)` 是否传入；检查工具参数是否为可序列化类型。Spring AI 2.0 不支持 `Optional`、`Future`、`Mono`、`Flux` 作为工具参数或返回值。

## 为什么 RAG 回答“资料中没有足够信息”？

说明检索没有召回满足阈值的文档，或 advisor 的 `allowEmptyContext(false)` 生效。先调用 `/search` 验证召回，再调整阈值、topK 或 filter。
