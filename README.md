<div align="center">
  <img src="document/images/chaste unicorn.png" width="200" style="vertical-align:middle;" alt="Chaste Unicorn"> 
  <span style="color:#b700ff;font-size: 80px;font-weight:bold">Unicorn</span>  
</div>
<div style="text-align: center">

[![AUR](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg)](https://github.com/lWoHvYe/unicorn/blob/main/LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/lWoHvYe/unicorn.svg?style=social&label=Stars)](https://github.com/lWoHvYe/unicorn)
[![GitHub forks](https://img.shields.io/github/forks/lWoHvYe/unicorn.svg?style=social&label=Fork)](https://github.com/lWoHvYe/unicorn)
[![Dependency Review](https://github.com/lWoHvYe/unicorn/actions/workflows/dependency-review.yml/badge.svg)](https://github.com/lWoHvYe/unicorn/actions/workflows/dependency-review.yml)
[![CodeQL](https://github.com/lWoHvYe/unicorn/actions/workflows/codeql-analysis.yml/badge.svg)](https://github.com/lWoHvYe/unicorn/actions/workflows/codeql-analysis.yml)
[![Gradle Package](https://github.com/lWoHvYe/unicorn/actions/workflows/gradle-publish.yml/badge.svg)](https://github.com/lWoHvYe/unicorn/actions/workflows/gradle-publish.yml)
</div>

本项目在eladmin项目的基础上，进行了部分扩展及尝试，在此表示感谢。

For lWoHvYe/unicorn project, will stay at Java 21 until 2025. will try Java 21+ in WHY-lWoHvYe/valentine-p2p

---

启动类 [AppRun.java](https://github.com/lWoHvYe/unicorn-starter/tree/master/src/main/java/com/lwohvye/AppRun.java)
和配置文件 [resources](https://github.com/lWoHvYe/unicorn-starter/tree/master/src/main/resources)
详见 [unicorn-starter](https://github.com/lWoHvYe/unicorn-starter)
模块。[启停脚本](script)。

**Java16**之后，默认强封装JDK内部类，详见[JEP 396](https://openjdk.java.net/jeps/396)
[JEP 403](https://openjdk.java.net/jeps/403)，需在启动时添加相关参数开启包访问。较简单的是添加
``--add-opens java.base/java.lang=ALL-UNNAMED`` ，也可根据需要缩小范围（在Java 9引入的JPMS/Jigsaw）。
详见：[Java 16](document/jdk/Java-16.md) [Java 17](document/jdk/Java-17.md)

**Java 19**起，引入Virtual Threads/Project Loom，相关[详见](document/jdk/Java-Preview-loom.md)

---

#### 引用方式 🎵

最新版本为: [![Maven Central](https://img.shields.io/maven-central/v/com.lwohvye/unicorn.svg?logo=github&style=flat)](https://mvnrepository.com/artifact/com.lwohvye/unicorn)

##### Maven

```xml

<project>

    <project.core.version>4.2.0-pi</project.core.version>

    <!--    system模块    -->
    <dependency>
        <groupId>com.lwohvye</groupId>
        <artifactId>unicorn-security</artifactId>
        <version>${project.core.version}</version>
    </dependency>
    <!--   logging模块     -->
    <dependency>
        <groupId>com.lwohvye</groupId>
        <artifactId>unicorn-logging</artifactId>
        <version>${project.core.version}</version>
    </dependency>
</project>

```

##### Gradle

```groovy
// 4.x系列将基于Java 21, 部分module使用Kotlin, 使用Gradle build
ext { // 这个定义是可以传递的
    unicornVersion = '4.2.0-pi'
}

implementation "com.lwohvye:unicorn-security:$unicornVersion"

// 引入滑动captcha
implementation("com.lwohvye:unicorn-security:$unicornVersion") {
    capabilities {
        requireCapability('com.lwohvye:unicorn-security-captcha')
    }
}
// 引入custom-log
implementation("com.lwohvye:unicorn-security:$unicornVersion") {
    capabilities {
        // 这里只支撑横线，不支持驼峰
        requireCapability('com.lwohvye:unicorn-security-business-log')
    }
}
```

---

#### 项目简介

一个基于最新的Java 21(17) 版本、 Spring Boot 3.2、 Jpa、 Spring Security、RabbitMQ、Vue的前后端分离的脚手架。
在各模块基本解耦之后，可根据需要只引入部分模块实现相关职能。

#### 项目源码

|            | 后端源码                               | 前端源码                                   |
|------------|------------------------------------|----------------------------------------|
| 原项目-github | https://github.com/elunez/eladmin  | https://github.com/elunez/eladmin-web  |
| 原项目-码云     | https://gitee.com/elunez/eladmin   | https://gitee.com/elunez/eladmin-web   |
| github     | https://github.com/lWoHvYe/unicorn | https://github.com/lWoHvYe/unicorn-web |

#### 主要特性

- 使用最新技术栈，社区资源丰富，基于Java 21(Core Module Support 17-21)、Spring Boot 3.2。
  (Support Virtual Threads/fibre/loom)
- 基于注解的动态查询（Specification），可根据需要扩充查询注解。
- 支持接口级别的功能权限，动态权限控制
- 支持数据字典，可方便地对一些状态进行管理
- 高效率开发，代码生成器可一键生成前后端代码
- 对一些常用前端组件封装：表格数据请求、数据字典等
- 前后端统一异常拦截处理，统一输出异常，避免繁琐的判断
- 使用ShardingSphere实现多数据源和读写分离。该方式针对MySQL数据库。对系统侵入性小。（只需引入依赖，并在yaml中配置数据源信息即可）
  [unicorn-starter](https://github.com/lWoHvYe/unicorn-starter)。
- 整合Redisson拓展Redis的功能，读写分离
- 整合消息队列RabbitMQ，实现消息通知、延迟消息，服务解耦。
- 各模块独立，基本可插拔：若只需查询注解等基础功能，只需引入Core模块即可，Beans, Security, Logging, 3rd Tools, Code Gen
  模块可插拔， 除了传统To B业务，还可用于To C业务（see [OAuth2.0 part](unicorn-oauth2) ）

#### 系统功能

- 用户管理：提供用户的相关配置，新增用户后，默认密码为123456
- 角色管理：对权限与菜单进行分配，菜单权限、数据权限(Draft)、接口权限(_In Progress_)
- 菜单管理：已实现菜单动态路由，后端可配置化，支持多级菜单
- 部门管理：可配置系统组织架构，树形表格展示(Draft)
- 岗位管理：配置各个部门的职位(Draft)
- 字典管理：可维护常用一些固定的数据，如：状态，性别等
- 系统日志：记录用户操作日志与异常日志，方便开发人员定位排错
- 定时任务：整合Quartz做定时任务，加入任务日志，任务运行情况一目了然
- 代码生成：高灵活度生成前后端代码，减少大量重复的工作任务（逆向有很多方案，这种基于template的有一定的灵活性）
- 邮件工具：配合富文本，发送html格式的邮件

#### 项目结构

项目采用按功能分模块的开发方式，结构如下

- `unicorn-core` 系统的Core模块，BaseClass及各种Util，(基于Multi-Release JAR Files，Support Java 17 - 21)

- `unicorn-beans` 基础Beans的Definition及Configuration，To C业务可只引入该dependency

- `unicorn-sys-api` Sys Module基础实体及API，方便服务拆分

- `unicorn-security` 系统权限模块，包含权限配置管理等。

- `unicorn-logging` 系统的日志模块，其他模块如果需要记录日志需要引入该模块，亦可自行实现

- `unicorn-tp-tools-kotlin` 第三方工具模块，包含：邮件、S3，可视情况引入

- `unicorn-code-gen-kotlin` 系统的代码生成模块。这部分待优化，亦非必须模块

- `unicorn-starter` [启动类(Maven)，项目入口，包含模块及组件配置（DB读写分离 + Cache读写分离）](https://github.com/lWoHvYe/unicorn-starter)

- `valentine-starter` 启动配置示例(Gradle)，尝试Kotlin/Kotlinx

- `unicorn-oauth2` OAuth2 Sample，AuthorizationServer, OAuth2Client + Gateway, ResourceServer

#### 详细结构

```
- unicorn-core 公共模块
    - annotation 为系统自定义注解
    - base 提供了Entity、Service、DTO基类和mapstruct的通用mapper
    - exception 项目自定义异常类
    - utils 系统通用工具类, security, json, rabbitmq, redis,...
        - QueryHelp 基于Annotation的JPA动态查询Specification
        - SpringContextHolder 自定义SpringUtil
        - JDKUtils, UnsafeUtils
        - XRabbitAbstractProducer, YRabbitAbstractConsumer
        - SecurityUtils, ReactiveSecurityUtils
- unicorn-beans 基础Bean
    - advice 统一数据返回及异常处理
    - config 基础配置，Security配置，redis配置，openApi配置，Rsa配置等
        - security 权限控制，为swarm化，提供全局关闭Security功能
        - UnicornAutoConfiguration: 自动化装配
        - ValentineExecutorConfig: Running Spring Applications (Servlet Web Servers & Task Execution) on Virtual Threads Before Spring Boot 3.2
- unicorn-sys-api 基础实体及DTO
    - modules 基础实体及接口定义
- unicorn-security 系统核心模块
	- common 配置跨域、静态资源、数据权限、实体表映射、系统完成入口
	    - config 
	    - init 容器启动后的钩子call back
	    - orm jpa-entity的部分配置，eg: Table Mapping
	    - web corsFilter configurer and so on
	- modules 系统相关模块(登录授权、消息队列、系统监控、定时任务、运维管理等)
	    - infrastructure business log相关
	    - mnt.websocket WebSocket
	    - quartz 定时任务
	    - rabbitmq 消息队列相关
	    - security 核心权限控制
	    - system 用户-权限管理
- unicorn-logging 系统日志模块
    - aspect 基于Anno的log切面
    - rabbitmq Async log
- unicorn-tp-tools-kotlin 系统第三方工具模块(kotlin)
- unicorn-code-gen-kotlin 系统代码生成模块(kotlin)
```

#### 运行环境

- Java 21 基础运行环境
- Mysql 5.7/8.0 数据库 读写分离/单数据源-通过配置数据源的方式切换
- Redis 7.2 缓存
- RabbitMQ 发布-订阅（解耦、异步）
- ELK 日志系统，config for prod env
- 可基于docker [一键搭建](document/docker/docker-compose-env.yml)。当然目录还是要自己建的，另外RabbitMQ记得装延迟插件，ES记得装IK
- 若想搭建k8s版，可[参照](https://github.com/lWoHvYe/mall-swarm/tree/main/document/kubernetes/env) ,这个是基于NFS挂载的。

#### 特别鸣谢

- [![JetBrains Logo (Main) logo](https://resources.jetbrains.com/storage/products/company/brand/logos/jb_beam.svg)](https://jb.gg/OpenSourceSupport)

- 感谢 [JetBrains](https://www.jetbrains.com/) 提供的非商业开源软件开发授权。

- 感谢 [PanJiaChen](https://github.com/PanJiaChen/vue-element-admin) 大佬提供的前端模板

- 感谢 [Moxun](https://github.com/moxun1639) 大佬提供的前端 Curd 通用组件

- 感谢 [zhy6599](https://gitee.com/zhy6599) 大佬提供的后端运维管理相关功能

- 感谢 [j.yao.SUSE](https://github.com/everhopingandwaiting) 大佬提供的匿名接口与Redis限流等功能

- 感谢 [d15801543974](https://github.com/d15801543974) 大佬提供的基于注解的通用查询方式

- 感谢 [elunez](https://github.com/elunez) 大佬提供的eladmin项目

---

#### Feature list

- dev_4.0 JPMS改造（3.0版本有做部分尝试，当前在IDEA中可开发调试，但模块化打包部署尚未以Named Module的方式运行，
  推测是Spring Boot的 ClassLoader下全是Auto-Module）
- Resource管理页面(partly，角色-资源管理)，delay
- swarm化，可以参考[why-swarm (已停工，计划重构 toC + OAuth2.0)](https://github.com/WHY-lWoHvYe/why-swarm)

#### TODO

- OAuth 2.0 (_In Progress_)
