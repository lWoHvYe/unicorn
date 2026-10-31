# unicorn-ai-support 技术选型决策（ADR 摘要）

> 日期：2026-10-31  
> 范围：`unicorn-ai-support` 的传输层、分布式原语、韧性、可观测性和 RAG/工具编排

## ADR-001：不引入 Netty/WebFlux 重写

**状态：接受**

当前应用是 Spring MVC + JDBC + Spring AMQP + Spring Data Redis 的阻塞式业务应用；SSE 用 `SseEmitter`，并启用虚拟线程。

- Netty 是传输框架，不是更高层的业务抽象。
- 改成 WebFlux/Reactor Netty 会要求 RAG、工具、审批、JDBC 全部非阻塞，属于重写。
- 当前规模下，Spring MVC + 虚拟线程 + `SseEmitter` 更简单、更易排障。

**重新评估条件**：SSE 长连接达到单实例万级、需要独立流式网关时，另建 WebFlux 网关，不重写核心领域。

## ADR-002：不整体引入 Redisson

**状态：接受**

support 已有 Spring Data Redis/Lettuce。Redisson 适合 `RLock`、`RRateLimiter`、Near Cache、延迟队列，但：

- outbox/审批依赖数据库事务和 CAS，Redisson 锁不能替代数据库一致性。
- ChatMemory 以 JDBC 为 source of truth，Redis 不应替换持久化。
- 当前限流是一个很小的 fail-closed Lua；引入 Redisson 会新增第二套 Redis 客户端和配置。

**决策**：保留 `RateLimitStore` 接口；未来只有在统一 Redisson 客户端并需要分布式锁/Near Cache 时增加 `RedissonRateLimitStore`。

**重新评估条件**：需要跨实例分布式锁、Near Cache、延迟队列，或 Redis 客户端统一为 Redisson。

## ADR-003：采用 Resilience4j 保护模型调用

**状态：接受**

`SupportModelInvoker` 使用 Resilience4j Bulkhead + Retry + CircuitBreaker：

- Bulkhead 限制并发模型调用。
- Retry 只重试模型失败，`NoContextException` 不重试。
- CircuitBreaker 防止供应商持续故障时拖垮线程。
- 保留虚拟线程 timeout 作为 HTTP 层之外的硬边界。

**限制**：当前 fallback 绕过主 circuit，避免主 circuit open 时 fallback 被一起拒绝；provider 级独立熔断仍可后续增强。

## ADR-004：本地限流使用 Caffeine

**状态：接受**

`LocalRateLimitStore` 原使用无界 `ConcurrentHashMap`，按 key 永久增长。改为 Caffeine：

- `maximumSize(100_000)`
- `expireAfterWrite(2h)`
- 窗口时间仍由 `Window.startedAt` 控制

Redis 模式继续保留 Lua 固定窗口，不改语义。

## ADR-005：采用 Micrometer Tracing + OTLP

**状态：接受**

加入 `micrometer-tracing-bridge-otel` 与 `opentelemetry-exporter-otlp`：

- `management.tracing.sampling.probability` 默认 0.1。
- `OTEL_EXPORTER_OTLP_ENDPOINT` 配置 collector。
- Spring AI 的 Observation、HTTP、工具、outbox 可通过同一 trace 关联。

**限制**：Grafana dashboard、SLO error budget 和 collector 部署仍待补齐。

## ADR-006：Spring Modulith / ShedLock 暂不替换

**状态：暂缓**

- Spring Modulith Event Publication Registry 可替代自研 outbox，但迁移涉及事件模型和存储约定；当前先修复 outbox claim token 的多实例正确性。
- ShedLock 适合多实例定时任务；当前 outbox 已用数据库 claim token 保证单 owner，暂不引入。
- 若定时任务增多或需要统一调度锁，再评估 ShedLock。

## ADR-007：保留 Spring AI 2 Advisor/ChatMemory

**状态：接受**

- `MessageChatMemoryAdvisor`、`RetrievalAugmentationAdvisor`、`SafeGuardAdvisor` 已覆盖 chat memory、RAG、安全。
- 自研 `SupportDocumentRetriever`/`SupportQueryAugmenter` 只保留业务差异：tenant ACL、混合检索、citation 格式、空上下文拒答。
- 后续可补 `ToolCallingAdvisor`/`ToolCallLimits` 和结构化输出，不重写现有 RAG。

## ADR-008：不把 StructuredTaskScope 作为唯一生产路径

**状态：接受**

JDK 25 中 `StructuredTaskScope` 仍为 preview。当前使用虚拟线程/固定 Executor 完成并行；如果后续用于 query expansion/vector+lexical 并行，应：

- 加 feature flag；
- 提供 ExecutorService fallback；
- 明确 preview 运行时参数；
- 有取消/超时/异常测试。
