# RAG 运维手册

## 文档导入

推荐使用 `/api/ai/rag/documents` 上传 Markdown、TXT、PDF、DOCX 等文件。服务根据扩展名选择 reader：Markdown 使用 `MarkdownDocumentReader`，TXT 使用 `TextReader`，其他格式交给 Apache Tika。

导入前会为每个文档补齐 `source`、`tenant`、`docType`、`ingestedAt`、`contentHash` 元数据。切分后每个 chunk 继承这些元数据，便于按租户或来源过滤。

## 切分策略

默认 chunkSize 为 800 token，minChunkSizeChars 为 350。中文标点 `。！？；` 与英文标点都会作为候选断句点。chunk 太小会导致语义不完整，chunk 太大会稀释检索相关性，建议根据文档类型做 A/B 测试。

## 向量库

默认使用进程内 `SimpleVectorStore`，零外部依赖，重启后数据丢失。生产环境建议切换到 Redis Stack，并开启 RediSearch：

- 设置 `demo.ai.rag.vector-store=redis`。
- 设置 `REDIS_HOST`、`REDIS_PORT`。
- 索引名由 `demo.ai.rag.redis.index-name` 控制。
- `initializeSchema=true` 会自动创建向量索引。

## 检索质量排查

1. 先调用 `/api/ai/rag/search`，确认能召回正确文档。
2. 如果召回为空，降低 `demo.ai.rag.similarity-threshold` 或增加 `topK`。
3. 如果召回噪声大，缩小 topK、提高阈值，或使用 `filter` 限定 `tenant`/`docType`。
4. 如果问题口语化或含指代，先调用 `/api/ai/rag/query/rewrite` 或 `/query/expand`。
5. 如果模型仍然答非所问，检查 prompt 是否严格限制“只使用 context”。

## 成本与延迟

RAG 的一次问答通常包含一次 embedding 与一次 chat completion；查询改写与多查询扩展会额外增加 LLM 调用。可以通过缓存 embedding、限制 topK、对改写结果做短期缓存来控制成本。
