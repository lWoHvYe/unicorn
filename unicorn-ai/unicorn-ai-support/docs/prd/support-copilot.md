# Support Copilot PRD (v1)

## 目标
为企业客服提供多租户售后 copilot：知识检索、订单/库存工具、人工审批、转人工工单，并保留完整审计与质量闭环。

## 角色
- 租户管理员（TENANT_ADMIN）：租户、用户、知识、模型、配额、审计。
- 客服主管（SUPPORT_MANAGER）：知识发布、审批、工单、质量。
- 客服（SUPPORT_AGENT）：会话、检索、只读工具、发起审批/转人工。
- 外部 Agent：通过 MCP 使用受 scope 限制的只读/写工具。

## 核心流程
1. 客户/客服发起会话。
2. 系统按租户、知识库、ACL 检索文档，生成带 citation 的回答。
3. 只读工具直接执行；高风险写工具进入审批。
4. 低置信度/客户要求时创建人工工单并转入坐席。
5. 所有消息、工具、审批、知识变更、管理操作写审计。

## 非功能基线
- 单区域首期；100 租户、每租户 5k 文档、50 并发会话、日 5k 消息。
- API 月可用性 99.9%。
- 首 token p95 ≤ 2s；阻塞回答 p95 ≤ 8s。
- retrieval recall@5 ≥ 0.85；citation precision ≥ 0.95；groundedness ≥ 0.95。
- 跨租户数据泄漏为零。
- 模型/embedding 供应商故障时可降级或拒答，不产生无审计写操作。

## 安全与合规
- 租户隔离：应用过滤 + PostgreSQL RLS 双重保护。
- API Key 可轮换/吊销；JWT 校验 issuer/audience/scope。
- PII 可识别、脱敏、按留存策略删除/导出。
- 上传文件做 MIME/大小/恶意内容检查；原文进入对象存储。
- 提示注入、工具结果注入、输出审核纳入安全测试。

## 知识与质量
- 文档状态：UPLOADED → PARSED → REVIEW → ACTIVE → RETIRED。
- 文档版本、ACL、chunk 策略、embedding 模型版本可追溯。
- golden set 覆盖 retrieval、citation、groundedness、拒答、工具选择。
- 模型/提示词变更必须通过评测门禁。

## 工具与审批
- 工具按 scope/角色/租户授权；写操作需幂等、审批、审计，支持补偿。
- 审批有有效期；并发审批使用 CAS；审批结果写 `ai_approval`。
- 真实订单/库存/商品通过外部服务 adapter；本地 JDBC 表仅 local demo。

## 可观测性与 SLO
- 每次请求 trace 覆盖 HTTP、Retrieval、ChatModel、Embedding、VectorStore、Tool、RabbitMQ。
- 指标至少包含 chat latency、first token、retrieval empty rate、citation invalid rate、tool denial/approval latency、queue lag/DLQ、token/cost、SSE disconnect。
- 提供 dashboard、告警、runbook、error budget。

## 验收
- 跨租户/越权/ACL/MCP/文件安全测试通过。
- 并发退款/预留/审批/入库/聊天重试无双执行。
- golden set 指标达标。
- SSE 断开/超时/错误终态正确。
- 生产 K8s 滚动发布、迁移、备份恢复、回滚演练通过。
