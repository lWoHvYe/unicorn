# AGENTS.md

面向在本仓库中工作的编码代理（Claude Code、Codex、Copilot 等）。

`CLAUDE.md` 是早期说明，已在顶部标注以本文件为准；发生冲突时，以本文件、`settings.gradle.kts`、`build-logic/` 和实际构建结果为准。

> 最近核对：2026-10-30（工作区）。`./gradlew assemble` 通过；AI 新增生产参考 `unicorn-ai-support`（H2 本地 profile + PostgreSQL/pgvector/Redis/RabbitMQ infra profile、多租户、RAG citations、工具审批、MCP、指标）及 `unicorn-ai-tools` 领域/策略库；核心模块保留 `QueryHelp`/Redis 序列化/异常脱敏的针对性测试；support 已补 ACL、Redis 限流/缓存、JWT、SSE 工具事件、摘要、MMR/rerank、MCP 客户端认证与审批审计；P2 已补 JDBC 订单/库存/商品适配、token 成本、Docker 资产和 Testcontainers 集成测试；当前已落地 Java 25 ScopedValue、Spring AI 2 ChatMemory/RAG advisor、IAM/RLS、审计/outbox 和治理表。`./gradlew test` 全量仍失败属于预期，见 §8。

## 1. 项目速览

- **定位**：基于 eladmin 扩展的企业级 Java 脚手架/框架，模块可插拔；同时包含 To B 后台与 OAuth2 To C 样例。
- **Java**：`unicorn-core` 字节码基线为 **17**（Multi-Release JAR）；其余模块按当前 JDK（**25**）编译，无 MR-JAR。日常构建/运行使用 **JDK 25**。
- **关键版本**：Spring Boot **4.1.2**、Spring Cloud **2025.1.2**、Spring AI **2.0.1**、Gradle **9.8.2**、Kotlin **2.4.20**、Lombok 1.18.48、MapStruct 1.6.3、Redisson 4.8.0、jjwt 0.13.0。
- **坐标**：`com.lwohvye:*:4.8.4-chi`；rootProject 名称是 `valentine-p2p`（不是 `unicorn`）。
- **入口**：`com.unicorn.UnicornAppRun`，工程 `:valentine-starter-kotlin`。
- **硬约束**：所有 Java 编译任务带 `--enable-preview`；所有库模块都是 JPMS 模块（`module-info.java`），jar 同时带 `Automatic-Module-Name`。

## 2. 常用命令

```bash
./gradlew projects                            # 查看真实工程名/路径
./gradlew assemble                            # 可靠构建门槛（不跑测试）
./gradlew :valentine-starter-kotlin:bootRun   # dev profile，8081，默认 HTTPS
./gradlew :valentine-starter-kotlin:bootJar   # build/libs/valentine-starter-kotlin-4.8.4-chi.jar
./gradlew :valentine-starter-kotlin:bootBuildImage
./gradlew :unicorn-ai-chat:bootRun            # AI Chat demo，8092
./gradlew :unicorn-ai-rag:bootRun             # AI RAG demo，8093
./gradlew :unicorn-ai-mcp-server:bootRun      # MCP Server，8094
MCP_CLIENT_ENABLED=true MCP_API_KEY=demo-key ./gradlew :unicorn-ai-mcp-client:bootRun  # MCP Client，8095（server 需同时设置 MCP_API_KEY）
./gradlew :unicorn-ai-support:bootRun         # 生产参考，8096（默认 local/H2）
./gradlew publishToMavenLocal
./gradlew :unicorn-security:outgoingVariants  # 查看 business-log capability
./gradlew javaToolchains                      # 查看 17/21/25/26/27 toolchain
```

- **不要**在根目录直接 `./gradlew bootRun`：根工程没有该任务，而 10 个 Boot 子工程都有，必须写工程路径。
- 测试未专门维护，`./gradlew test` / `build` / `check` 失败属于**已知预期**，可直接忽略（§8；AI demo 模块的独立测试除外）；验证改动用 `assemble` + 针对性手工运行。
- `:unicorn-core` 提供 `java21Test`、`java25Test`、`java26Test`、`java27Test`，**没有 `java24Test`**；这些任务目前同样受 §8 影响。
- 首次构建或切换 JDK 时，Gradle 可能通过 foojay 解析/下载 toolchain（17/21/25/26/27），保持自动下载开启。
- 构建日志里的 `javadoc: 无效的标记: -enable-preview` 和 JPMS `模块 ... 同时从 ... 读取程序包` 是已知噪音，任务被配置为不失败；以 `BUILD SUCCESSFUL` 为准。
- 只处理部分模块：`-PexcludeProjects=unicorn-code-gen-kotlin,...`（`settings.gradle.kts` 会跳过对应 build 文件）。

## 3. 构建模型（改构建前必读）

`settings.gradle.kts` **自动发现** build 文件，不在 settings 中显式 `include`：

- 默认名 `build.gradle[.kts]`：工程路径 = 相对目录，例如 `:unicorn-oauth2:unicorn-mybatis`。
- 非默认名 `xxx.gradle.kts`：工程名 = 文件名去掉后缀，并**拍平到根**，例如
  `valentine-starter/valentine-starter-kotlin.gradle.kts` → `:valentine-starter-kotlin`；
  `unicorn-oauth2/unicorn-oauth2-gateway-client/...gradle.kts` → `:unicorn-oauth2-gateway-client`。
- 因此**目录名、build 文件名、工程路径三者可能不同**，写 Gradle 命令前先跑 `./gradlew projects`。

`build-logic/` 是 included build（不是 `buildSrc`），提供两个 precompiled convention plugin：

- `com.lwohvye.java-conventions`：设置 group/version、`java-library`、sources/javadoc jar、UTF-8、`--enable-preview`（JavaCompile/Test/JavaExec）、宽松 Javadoc。
- `com.lwohvye.publishing-conventions`：GitHub Packages 仓库、POM 元数据、条件签名、把 POM 塞进子模块 jar 的 `META-INF/maven`。

根 `build.gradle.kts` 还统一为所有 subproject 应用：

- freefair Lombok、MapStruct annotation processor（`mapstruct-processor` + `mapstruct-spring-extensions`）。
- `org.gradlex.extra-java-module-info`，并集中声明第三方自动模块名（`spring.security.core` 合并 `spring-security-web/access`、`jjwt.api`、`bizlog.sdk`、`easy.captcha`）。
- 根 `extra["sharedManifest"]` 被各模块 jar manifest 复用。

**依赖版本只改 `gradle/libs.versions.toml`**，不要在模块里写死版本（OAuth2 样例中的 MyBatis-Plus/GraalVM/R2DBC 等局部常量除外）。

## 4. 工程地图

| 目录 | Gradle 路径 | 类型 | 说明 |
| --- | --- | --- | --- |
| `unicorn-core` | `:unicorn-core` | 库 | 注解、基类、工具、`QueryHelp`、统一返回、Redis/MQ 抽象；MR-JAR 17/21/25/26/27 |
| `unicorn-beans` | `:unicorn-beans` | 库 | 自动装配、统一返回 advice、缓存、安全基础、虚拟线程执行器、MQ producer |
| `unicorn-sys-api` | `:unicorn-sys-api` | 库 | JPA 实体、DTO/Criteria、投影、`@HttpExchange` API（服务拆分契约） |
| `unicorn-security` | `:unicorn-security` | 库 | JWT 认证、接口级鉴权、system/quartz/websocket/rabbitmq；含 `businessLog` feature |
| `unicorn-logging` | `:unicorn-logging` | 库 | `@OprLog` 切面 + MQ 消费 + 日志查询 |
| `unicorn-tp-tools-kotlin` | `:unicorn-tp-tools-kotlin` | 库(Kotlin) | 邮件、验证码、S3/COS |
| `unicorn-code-gen-kotlin` | `:unicorn-code-gen-kotlin` | 库(Kotlin) | 基于 Freemarker 的代码生成 |
| `valentine-starter` | `:valentine-starter-kotlin` | 应用 | 启动示例（Java + Kotlin），端口 8081 |
| `unicorn-ai/unicorn-ai-chat` | `:unicorn-ai-chat` | 应用 | Spring AI ChatClient、流式、记忆（内存/Redis）、结构化输出、工具/Agent，端口 8092 |
| `unicorn-ai/unicorn-ai-rag` | `:unicorn-ai-rag` | 应用 | Spring AI RAG、ETL、Simple/pgvector/Redis Stack 向量库、RabbitMQ 异步入库、MySQL 任务审计，端口 8093 |
| `unicorn-ai/unicorn-ai-mcp-server` | `:unicorn-ai-mcp-server` | 应用 | MCP Server：`@McpTool`/`@McpResource`/`@McpPrompt`，端口 8094 |
| `unicorn-ai/unicorn-ai-mcp-client` | `:unicorn-ai-mcp-client` | 应用 | MCP Client：发现远程工具并注入 ChatClient，端口 8095 |
| `unicorn-ai/unicorn-ai-support` | `:unicorn-ai-support` | 应用 | **生产参考**：多租户 RAG/会话/工具审批/MCP，端口 8096 |
| `unicorn-ai/unicorn-ai-tools` | `:unicorn-ai-tools` | 库 | support 的订单/退款/库存领域、工具策略、幂等/审批/审计 |
| `unicorn-oauth2/unicorn-mybatis` | `:unicorn-oauth2:unicorn-mybatis` | 应用 | MyBatis-Plus 示例，端口 8082 |
| `unicorn-oauth2/unicorn-oauth2-authorizationserver` | `:unicorn-oauth2-authorizationserver` | 应用 | Spring Authorization Server，端口 9000 |
| `unicorn-oauth2/unicorn-oauth2-gateway-client` | `:unicorn-oauth2-gateway-client` | 应用 | Gateway + OAuth2 Client + Session，端口 8080 |
| `unicorn-oauth2/unicorn-oauth2-resource-server-mvc` | `:unicorn-oauth2-resource-server-mvc` | 应用 | MVC Resource Server，端口 8090，context-path `/res-mvc` |
| `unicorn-oauth2/unicorn-oauth2-resource-server-flux` | `:unicorn-oauth2-resource-server-flux` | 应用 | WebFlux + R2DBC Resource Server，端口 8091 |

### 模块依赖

```text
unicorn-core (baseline 17, MR-JAR)
 ├── unicorn-beans (21+)
 │    ├── unicorn-sys-api
 │    │    └── unicorn-security
 │    ├── unicorn-logging
 │    ├── unicorn-tp-tools-kotlin
 │    └── unicorn-code-gen-kotlin
 └── valentine-starter-kotlin ──> unicorn-security + unicorn-logging + unicorn-tp-tools-kotlin

unicorn-oauth2-* / unicorn-mybatis：独立 Boot 样例，不属于框架依赖链
unicorn-ai-*：独立 Spring AI 样例，不依赖框架库；`unicorn-ai-support` 是生产参考并依赖 `unicorn-ai-tools`
```

- `unicorn-security` 的 `businessLog` 是 Gradle feature variant：源码在 `src/businessLog/java`，能力坐标为
  `com.lwohvye:unicorn-security-business-log`（连字符）。消费方需：

  ```kotlin
  implementation(project(":unicorn-security")) {
      capabilities { requireCapability("com.lwohvye:unicorn-security-business-log") }
  }
  ```

  该源集没有 `module-info.java`，按 classpath 编译。
- `unicorn-oauth2-resource-server-mvc` 依赖的是**已发布**的 `com.lwohvye:unicorn-beans:4.8.3-chi`，不是本地 `project(":unicorn-beans")`；改本地 beans 不会影响它。
- OAuth2 的 5 个工程都是可独立 `bootRun`/`bootJar` 的样例，修改框架库时不要把它们当生产模块。

## 5. 核心机制

### 5.1 Multi-Release JAR 与虚拟线程

- `unicorn-core/unicorn-core.gradle.kts`：`multiRelease { targetVersions(17, 21, 25, 26, 27) }`。
- 基础源码 `src/main/java` 按 Java 17 编译；版本覆盖：
  - `src/main/java21`：`CallBack`、`ConcurrencyUtils`、`UnicornAbstractThreadUtils`
  - `src/main/java25`、`java26`、`java27`：仅 `ConcurrencyUtils`
  - **没有 `java24` 目录**。
- JVM 在类加载期按 `META-INF/versions/N` 选择同名类，不存在运行时探测；不要把需要 Spring 扫描的组件放进版本目录（MR-JAR 内的注解不会被正常处理）。
- `ConcurrencyUtils` 各版本使用 `StructuredTaskScope` 的不同 API（25/26 用 `open`/`Joiner`，27 改捕 `ExecutionException`），不要"统一简化"。
- `unicorn-beans` 未启用 MR-JAR，且直接使用 `Thread.ofVirtual()`，因此其编译基线至少 21。
- starter 打开 `spring.threads.virtual.enabled=true`；`ValentineExecutorConfig`/`VirtualThreadsPoolExecutor`/`VirtualThreadExecutor` 还会为 Tomcat/async/Redisson netty 安装虚拟线程执行器。

### 5.2 动态查询（`@Query` + `QueryHelp`）

- `unicorn-core` 的 `com.lwohvye.core.utils.QueryHelp` 扫描 criteria 字段上的 `@Query`，拼成 JPA `Specification`。
- 支持 `blurry` 多字段 OR 模糊、`joinName`（`roles>menus` 形式的多级 join）、`propName` 覆盖字段名，以及 20 种 `Type`（`IN`/`BETWEEN`/`LIKE_*`/`FUNCTION_*` 等）。
- 只支持 **AND** 组合；同一张表不支持多次不同 `join`；`IS_OR_NULL` 支持任意 `Number` 的 `-1`；`BETWEEN` 要求两个可比较值；`FUNCTION_4_EQUAL` 等库函数列无法走索引。
- `QueryHelp` 用 `ClassValue` 缓存 `@Query` 字段元数据，join 只在单次查询内复用；criteria 所在包必须 `opens` 给 `lwohvye.unicorn.core`，否则会直接报错而不是静默忽略条件。
- `IS_NULL`/`NOT_NULL` 的 **Boolean 字段是开关**：只有 `true` 才追加 `is null`/`is not null`，`null`/`false` 表示本次不限制；非 Boolean 字段保留原有“注解即生效”的语义。因此“先设 true、再置 null”的 `MenuQueryCriteria.pidIsNull` 不会残留条件。`IS_OR_NULL` 的 `-1` 支持任意 `Number`。
- Criteria/DTO 放在 `unicorn-sys-api`；`valentine-starter` 另有一份 Kotlin 版 `QueryHelp`，已同步 `IS_NULL`/`IS_OR_NULL`/空集合/`BETWEEN`/join 复用等关键修复，并有 `KotlinQueryHelpTest` 覆盖。
- `MenuServiceImpl.queryAll` 与 `DeptServiceImpl.queryAll` 统一：`isQuery=true` 时默认仅一级节点，传入其他过滤条件后取消 `pid is null`；导出场景不强制一级节点。

### 5.3 统一返回与异常

- `@RespResultBody` + `ResponseResultBodyAdvice` 把 `ResponseEntity` body 包装成 `ResultInfo`。
- `ResultInfo` 有三套互斥载体：`result`、`content`、`resultMap`；List 构造器把 `totalElements` 设为 `size()`。
- advice 的 `@ExceptionHandler` 是**全局**生效的（不依赖 `@RespResultBody`），统一输出 500/400/403/405。
- 返回 `String` 时输出 `ResultInfo` 的 JSON 文本，并设置 `application/json`；不再使用 `ResultInfo.toString()`。
- 兜底异常只返回固定文案 + `traceId`，不暴露 `ex.getMessage()`；`DataIntegrityViolation` 返回固定“数据完整性校验失败”。
- 在 Filter/Handler 等非 Controller 场景用 `ResultUtils.resultJson(...)`。

### 5.4 认证与鉴权（最容易踩坑）

- **登录不是 Controller**：`MyCustomDsl` 把 `CustomAuthenticationFilter`/`CustomAuthenticationCaptchaFilter` 注册在 `/auth/login`，仅处理 JSON；密码经 RSA 私钥解密；验证码由 `com.anji` 实现并通过 `CaptchaCacheServiceRedisImpl` 存 Redis。
- **JWT**：`TokenProvider` 使用 HS 密钥（`jwt.base64-secret`），claims 只放用户名，**不放权限**；每个请求重新 `loadUserByUsername`（有 Caffeine 本地缓存）以获取最新权限。
- Token 失效只有两条路径：过期、改密（`pwdResetTime`）。**没有 online-user 缓存，logout 也不会服务端吊销**；`singleLogin` 配置当前无实现。
- **接口级鉴权**：`CustomInvocationSecurityMetadataSource` 读 `sys_resource`（pattern + reqMethod + roleCodes），命中后要求 `ROLE_<roleCode>`；`CustomAuthorizationManager` 逐条匹配。
- `@AnonymousAccess` / `@AnonymousXxxMapping` 的语义是返回 `ROLE_ANONYMOUS`，**不是 permitAll**；请求仍会经过 JWT 过滤器。
- **未登记到 `sys_resource` 的接口默认返回 `ROLE_LOGIN`（登录即可访问）**。新增需要权限控制的接口时，必须补资源记录，否则等于向所有登录用户开放。
- `local.sys.sim-auth=true` 会启用 `SimpleSecurityConfig`；其中 `DRAW = RandomUtil.randomBoolean()` 会在"全部放行"和"x509"两条链间**随机**选择，不要用于可重复环境。
- `SecurityUtils.getCurrentUser()` 每次 `loadUserByUsername` 是为了拿到最新动态权限，`UserLocalCache` 的 Caffeine 缓存负责降低 DB 压力；不要简单改成只读 JWT principal，否则权限变更会延迟。`getCurrentUsername()` 已优先从 principal 取用户名。
- 密码编码器为 BCrypt；默认账号与权限数据来自 `sql/unicorn.sql`。

### 5.5 缓存

- `RedisConfig` 注册 `@Primary` 的 `RedissonSpringCacheManager`，配置文件为 **classpath:`cache-config.yaml`**（由应用提供，starter 已有；beans 模块自身不带）。
- Redis value 使用 Jackson `DefaultTyping.NON_FINAL` + `@class` 类型信息；该格式与旧版 `JAVA_LANG_OBJECT` 不兼容，升级后需要清理一次对应缓存。`RedisConfigTest` 覆盖 List/Map/POJO 的 round-trip。
- Key 生成器带 `local.sys.name` 命名空间：`-n_n-<name>-n_n-::类::方法::sha256(参数)`；改 `local.sys.name` 等于整体换缓存命名空间；类名使用 `AopUtils.getTargetClass`，避免代理类名进入 key。
- `SysCacheKey` 中的前缀必须与 `@Cacheable(key = "...")` 完全一致，`RedisUtils.delInRC(...)` 才能清到；否则会静默清不掉。
- 放入 Redis 的集合必须是可变且可 JSON 序列化的，使用 `new ArrayList<>(...)`，不要用 `Stream.toList()`。
- `localCacheManager`（`ConcurrentFreshCacheManager`）会对值做 Jackson 深拷贝，值同样必须可序列化。

### 5.6 消息队列

- 交换机/队列/绑定集中在 `unicorn-security` 的 `RabbitMQConfig`；延迟消息依赖 RabbitMQ 的 `x-delayed-message` 插件。
- 生产端继承 `XRabbitAbstractProducer`；`SimpleMQProducerService` 是 `sealed`，用户可继承 `ExtensionProducerService` 覆盖默认 producer。
- 消费端继承 `YRabbitAbstractConsumer`：按 `msgType` 过滤、忽略本实例消息、失败只重投一次。
- 登录成功/失败、操作日志、business log 都走 MQ；没有 RabbitMQ 时相关功能不可用（本地起服务前先确认 broker）。

### 5.7 JPMS

- 每个库模块都有 `module-info.java`；Spring 组件扫描依赖 `exports ... to spring.beans, spring.aop, spring.web`，属性绑定/反射依赖 `opens ... to spring.core`。
- **新增包/Controller/Entity 时必须同步修改对应 `module-info.java` 的 `exports`/`opens`**，否则 Spring 扫不到或反射失败。
- `exports` 与 `opens` 没有包含关系；只 export 不 open 时 Jackson/JPA 仍可能失败。
- `unicorn-core` 只 `opens` base 和 utils 系列（utils 仅对 `spring.core`）；新增反射访问要补 `opens`。
- jar manifest 中的 `Automatic-Module-Name` 为 `lwohvye.<module-name-with-dots>`；`unicorn-core` 的模块名是 `lwohvye.unicorn.core`，starter 是 `lwohvye.valentine.starter`。

### 5.8 JPA

- `BaseEntity` 提供 `createBy/updateBy/createTime/updateTime`，由 `AuditorConfig` + `@EnableJpaAuditing` 填充。
- 实体/仓库扫描在 `UnicornAutoConfiguration`：`com.lwohvye.api.modules.*.domain`、`com.lwohvye.sys.modules.*.domain`、`com.lwohvye.*.domain`；repository 同理。新实体放在这些模式覆盖的包下。
- 表名支持 `#{local.sys.table-name.xxx:sys_xxx}` 形式，由 `ServerAwareNamingStrategy` 解析；使用该机制后不要写原生 SQL，改用 HQL。
- `@DynamicInsert` 下 `save()` 返回的实体不包含数据库默认值；`@EntityGraph` + 分页会退化为内存分页。
- `BaseService.doInit()/doRegister()` 默认抛 `NeedImplementException`，子类必须实现。

### 5.9 Kotlin 模块

- `unicorn-tp-tools-kotlin`、`unicorn-code-gen-kotlin`、`valentine-starter` 使用 Kotlin JVM/Spring/JPA/Lombok 插件，`languageSettings.apiVersion = "2.0"`（构建会有 Kotlin 2.4 的弃用警告）。
- 代码生成器从**当前工作目录**读取 `generator.properties`（不是 classpath），运行/调试时工作目录必须包含该文件。
- `valentine-starter` 演示了 `VirtualThreadCoroutineDispatcher`、`Channel/Flow/Actor`、API 版本化路由等用法，属于学习/示例代码。

## 6. 运行与配置

- **starter**：`server.port=8081`，默认 **HTTPS**（`server.ssl.bundle: my-pem-bundle`，证书在 `src/main/resources/crt/`），默认 profile `dev`，`spring.jpa.hibernate.ddl-auto=none`。
- **profile**：只有 `application-dev.yml` 和 `application-sit.yml`，没有 prod。外部依赖默认指向 `10.211.55.x`，可用 `DB_HOST/DB_PORT/DB_NAME/DB_USER/DB_PWD`、`CACHE_PORT`、`MQ_PORT` 覆盖。
- **读写分离**：只有 Redis 主从（`sit` 的 `masterSlaveServersConfig`，`readMode: SLAVE`），**没有 ShardingSphere**，数据库是单数据源。
- **数据库**：`sql/unicorn.sql`（19 张表 + 种子数据，含 admin 用户、角色、菜单、资源、定时任务）。首次启动前先建库导入。
- **基础设施**：`document/docker/docker-compose-env.yml`（MySQL 主 + 2 从、Redis 主从、RabbitMQ 集群）或 `docker-compose-env-sim.yml`（单机）。RabbitMQ 需安装延迟插件。
- **OAuth2 样例拓扑**：`authorizationserver:9000`（授权状态存 Redis db4）→ `gateway-client:8080`（Session + TokenRelay）→ `resource-server-mvc:8090` / `resource-server-flux:8091`。
- **AI Demo（Spring AI 2.0.1）**：`unicorn-ai/` 下 4 个独立 demo，Chat 8092、RAG 8093、MCP Server 8094、MCP Client 8095；`unicorn-ai-support` 8096 是生产参考（多租户、citations、工具审批、审计、MCP）。模型通过 `OPENAI_API_KEY` / `OPENAI_BASE_URL` / `OPENAI_CHAT_MODEL` 配置；support 的 local profile 使用 H2 + SimpleVectorStore + 本地演示模型，无需 key。
  - `--spring.profiles.active=infra` 连接现有 dev 基础设施：Chat 用 Redis（10.211.55.29:6379 / redis-psd，db 6）持久化对话记忆；RAG 用 Redis Stack 向量库（索引 `unicorn-ai-rag`，维度 1536）、MySQL（10.211.55.29:3306/unicorn，root/root）做异步入库任务审计、RabbitMQ（10.211.55.29:5672，admin/admin，vhost /unicorn）做异步入库。
  - RAG 向量库可选 `simple` / `redis` / `pgvector`；infra 默认 `redis`，备选 pgvector 用 `--demo.ai.rag.vector-store=pgvector`。RedisVectorStore 启动时通过固定维度包装避免调用 embedding API。
  - 当前 Redis Stack 已加载 search/ReJSON；若换成普通 Redis，需改用 pgvector 或 simple，否则会报 `FT._LIST` 不存在。
  - MCP 场景先启动 server，再以 `MCP_CLIENT_ENABLED=true` 启动 client。旧 `unicorn-ai-mcp-server` 现在也要求 `X-API-Key`（需显式设置 `MCP_API_KEY`），但仍是单租户 demo；生产 MCP 以 `unicorn-ai-support` 的 `/mcp` 为准（API Key/JWT + 租户隔离）。`unicorn-ai-mcp-client` 支持 `DEMO_AI_MCP_CLIENT_API_KEY` 和 `DEMO_AI_MCP_CLIENT_ALLOWLIST`。完整 curl 示例见 `unicorn-ai/README.md` 和 `unicorn-ai/unicorn-ai-support/README.md`。
- **密钥**：RSA、JWT、Redis、RabbitMQ 口令目前明文写在 `application*.yml` 中；不要新增明文密钥，优先用环境变量，也不要把这些值复制到文档/日志。
- **脚本**：`script/*.sh` 仍使用 `valentine-starter.jar` 和 `JAVA_HOME=jdk-21`，与当前产物 `valentine-starter-kotlin-4.8.4-chi.jar` 和 JDK 25 不一致；只能当模板改后使用。`docker-compose.yml` 的 `valentine-starter:3.0.0` 同样过时。

## 7. 开发约定 / 修改检查清单

- 文件头保留 Apache-2.0 license header；注释以中文为主，保持现有风格。
- 新依赖：加到 `gradle/libs.versions.toml`；若产生新的自动模块，在根 `build.gradle.kts` 的 `extraJavaModuleInfo` 中声明模块名。
- 新模块：确认 build 文件名与工程路径（§3）；补 `module-info.java`、`Automatic-Module-Name`、POM 发布块。
- 新包：同步 `exports`/`opens`。
- 新 REST 接口：需要鉴权就在 `sys_resource` 登记 pattern/method/role；确实匿名才用 `@AnonymousXxxMapping`。
- 新实体：放在 `unicorn-sys-api` 的 `domain` 包（或符合扫描模式的包），DTO/Criteria 同模块。
- 新缓存：在应用的 `cache-config.yaml` 登记 cacheName，并保持 `SysCacheKey` 前缀一致。
- 新 MQ：在 `RabbitMQConfig` 声明 exchange/queue/binding，生产/消费端继承抽象基类。
- 测试：JUnit 5，文件名 `*Test.java`；历史测试失败可忽略（§8），新增测试不要依赖本地磁盘路径、外部 DB 或 GraalJS。
- 提交前至少跑 `./gradlew assemble`；涉及 starter 时再跑一次 `:valentine-starter-kotlin:bootRun` 或 `bootJar`；涉及 AI demo 时跑对应的 `:unicorn-ai-*:assemble` 并至少验证 `/actuator/health`。

## 8. 已知偏差与雷区

### 8.1 测试未维护，失败属于预期（可忽略）

仓库没有专门维护测试；`./gradlew test --continue` 实测失败，这是已知状态，**不代表业务代码回归，可直接忽略**。原因：
1. `unicorn-security`、`unicorn-oauth2-resource-server-flux`、`unicorn-oauth2-authorizationserver` 等模块**没有配置 `useJUnitPlatform()`**，Gradle 9.8 对"有测试源码但发现不到测试"直接报错；`unicorn-core`/`unicorn-beans` 已配置平台与针对性测试，见下方例外。
2. `valentine-starter` 的测试依赖外部环境：
   - `ScriptEngineTest` 需要可用的 GraalJS ScriptEngine，当前 NPE；
   - `QueryHelpTest` 是 `@SpringBootTest`，需要真实数据库方言，当前 Hibernate `DialectFactoryImpl` 失败。
3. `unicorn-core` 的 `FileUtilsTest` 还包含机器相关的硬盘容量断言和已不存在的历史 jar 路径；`TestVarargs` 是演示 MethodHandle 失败场景，已从 `test` 任务排除。

因此：**把 `./gradlew assemble` 作为构建门槛**，不要为了让 `test` 变绿去改业务代码；只有在任务明确要求补测试时，再给 Java 模块加 `useJUnitPlatform()`、JUnit 依赖，并移除环境相关断言。

例外：`unicorn-ai-tools:test`（6 个）、`unicorn-ai-support:test`（21 个通过；Testcontainers/外部 PG/Redis/RabbitMQ 测试默认 skip；`AI_INFRA_TEST=true` 时外部集成与 RLS 非 owner 测试通过）和 `unicorn-ai-mcp-client:test`（1 个）不依赖真实模型 key；`unicorn-core` 的 `QueryHelpTest`（9 个）、`unicorn-beans` 的 `RedisConfigTest`（3 个）和 `ResponseResultBodyAdviceTest`（5 个）不依赖外部服务，可通过：
`./gradlew :unicorn-core:test :unicorn-beans:test`；`valentine-starter` 的 `KotlinQueryHelpTest`（5 个）可通过 `./gradlew :valentine-starter-kotlin:test --tests com.unicorn.utils.KotlinQueryHelpTest`。AI demo 的独立测试同样可通过；这不改变全量 `test` 仍失败的事实。

### 8.2 文档与脚本漂移

| 位置 | 过时内容 | 现状 |
| --- | --- | --- |
| `CLAUDE.md` | `buildSrc`、`java24Test`、根目录 `bootRun`、ShardingSphere 读写分离、Java 17-25 | 顶部已标注以 `AGENTS.md` 为准；实际是 `build-logic`、无 java24、必须带工程路径、无 ShardingSphere、支持到 27 |
| `README.md` | 版本 `4.8.3-chi`、Java 24、ShardingSphere、`unicorn-starter` Maven 模块 | 当前 `4.8.4-chi`、Java 25、无 ShardingSphere、Maven starter 在独立仓库 |
| `script/*.sh` | `valentine-starter.jar`、`jdk-21` | 产物为 `valentine-starter-kotlin-4.8.4-chi.jar`，JDK 25 |
| `docker-compose.yml` | `valentine-starter:3.0.0` | 与当前工程名/版本不一致 |
| `document/jdk/Java-16.md`、`Java-17.md`、`springboot/2.x`、`Tips.md` | 旧 JDK/Maven/sharding 说明 | 仅作历史参考 |

### 8.3 其他雷区

- `@AutoConfiguration` 只登记了 `UnicornAutoConfiguration`；`ValentineExecutorConfig`、`VirtualThreadsPoolExecutor` 等虽有注解但未进 `AutoConfiguration.imports`，靠 component scan 生效，排序语义与标准自动配置不同。
- `SimpleSecurityConfig.DRAW` 的随机分支会让 `sim-auth` 环境的鉴权行为不可重复。
- Redis 缓存序列化从 `JAVA_LANG_OBJECT` 改为 `NON_FINAL` 后，旧格式的缓存值需要清理，避免升级后首次读取反序列化失败。
- `RedisUtils` 的普通操作走 JSON 序列化的 `RedisTemplate`，Lua 锁走 `StringRedisTemplate`；同一 key 的文本表示不同。
- `ResultInfo` 的 `content`/`resultMap` 依赖字段命名约定，改造返回结构时要同时检查前端/调用方。
- `spring-boot-starter-*` 在 Boot 4 中已改名（如 `spring-boot-starter-webmvc`、`spring-boot-starter-restclient`）；JSON 栈为 Jackson 3（`tools.jackson.*`），不要按 Boot 3 的依赖名/包名改代码。

## 9. 关键文件索引

- 构建：`settings.gradle.kts`、`build.gradle.kts`、`gradle/libs.versions.toml`、`gradle/wrapper/gradle-wrapper.properties`、`build-logic/src/main/kotlin/*.gradle.kts`
- 核心：`unicorn-core/src/main/java/com/lwohvye/core/utils/QueryHelp.java`、`.../base/BaseEntity.java`、`.../utils/result/ResultInfo.java`、`.../utils/SecurityUtils.java`、`.../utils/redis/RedisUtils.java`、`.../utils/ConcurrencyUtils.java`、`unicorn-core/src/main/java/module-info.java`
- 自动装配：`unicorn-beans/src/main/resources/META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports`、`unicorn-beans/src/main/java/com/lwohvye/beans/config/UnicornAutoConfiguration.java`、`.../config/cache/RedisConfig.java`、`.../advice/ResponseResultBodyAdvice.java`
- 安全：`unicorn-security/src/main/java/com/lwohvye/sys/modules/security/config/SpringSecurityConfig.java`、`.../core/TokenProvider.java`、`.../core/filter/CustomInvocationSecurityMetadataSource.java`、`.../core/CustomAuthorizationManager.java`、`unicorn-security/src/main/java/module-info.java`
- 启动/配置：`valentine-starter/src/main/java/com/unicorn/UnicornAppRun.java`、`valentine-starter/src/main/resources/config/application.yml`、`application-dev.yml`、`application-sit.yml`、`valentine-starter/src/main/resources/cache-config.yaml`
- 数据：`sql/unicorn.sql`；基础设施：`document/docker/docker-compose-env.yml`
- OAuth2：`unicorn-oauth2/unicorn-oauth2-authorizationserver/src/main/java/sample/config/AuthorizationServerConfig.java`、`unicorn-oauth2/unicorn-oauth2-gateway-client/src/main/resources/application.yml`
- AI 生产参考：`unicorn-ai/unicorn-ai-support/docs/architecture-and-implementation.md`、`unicorn-ai/unicorn-ai-support/docs/prd/support-copilot.md`、`unicorn-ai/unicorn-ai-support/k8s/`、`.github/workflows/ai-support.yml`、`unicorn-ai/unicorn-ai-support/README.md`、`.../src/main/java/com/unicorn/ai/support/chat/SupportChatService.java`、`.../knowledge/DocumentIngestionService.java`、`.../retrieval/RetrievalService.java`、`.../retrieval/QueryPlanService.java`、`.../retrieval/RerankService.java`、`.../security/JwtDecoderConfig.java`、`.../ratelimit/RedisRateLimitStore.java`、`.../config/CacheConfig.java`、`.../tools/SupportToolFacade.java`、`unicorn-ai/unicorn-ai-tools/src/main/java/com/unicorn/ai/tools/policy/ToolInvocationService.java`
- AI demo：`unicorn-ai/README.md`、`unicorn-ai/unicorn-ai-chat/src/main/java/com/unicorn/ai/chat/config/ChatClientConfig.java`、`.../memory/RedisChatMemoryRepository.java`、`.../tools/ShopTools.java`、`unicorn-ai/unicorn-ai-rag/src/main/java/com/unicorn/ai/rag/ingestion/DocumentIngestionService.java`、`.../service/RagService.java`、`unicorn-ai/unicorn-ai-rag/src/main/java/com/unicorn/ai/rag/config/VectorStoreConfig.java`、`.../config/PgVectorProperties.java`、`.../async/AsyncIngestPublisher.java`、`.../job/JdbcIngestJobRepository.java`、`unicorn-ai/unicorn-ai-mcp-server/src/main/java/com/unicorn/ai/mcp/server/tools/OrderMcpTools.java`、`unicorn-ai/unicorn-ai-mcp-client/src/main/java/com/unicorn/ai/mcp/client/config/McpClientConfig.java`

---

维护本文件时，请同步更新"最近核对"日期、工程地图和已知偏差；若修改了构建命令或测试配置，务必实际跑一遍再写结论。
