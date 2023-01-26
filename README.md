<div align="center">
  <img src="document/images/chaste unicorn.png" width="200" style="vertical-align:middle;" alt="Chaste Unicorn"> 
  <span style="color:#b700ff;font-size: 80px;font-weight:bold">Unicorn</span>  
</div>
<div style="text-align: center">

[![AUR](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg)](https://github.com/lWoHvYe/eladmin/blob/main/LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/lWoHvYe/eladmin.svg?style=social&label=Stars)](https://github.com/lWoHvYe/eladmin)
[![GitHub forks](https://img.shields.io/github/forks/lWoHvYe/eladmin.svg?style=social&label=Fork)](https://github.com/lWoHvYe/eladmin)
[![Dependency Review](https://github.com/lWoHvYe/eladmin/actions/workflows/dependency-review.yml/badge.svg)](https://github.com/lWoHvYe/eladmin/actions/workflows/dependency-review.yml)
[![CodeQL](https://github.com/lWoHvYe/eladmin/actions/workflows/codeql-analysis.yml/badge.svg)](https://github.com/lWoHvYe/eladmin/actions/workflows/codeql-analysis.yml)
[![Maven Package](https://github.com/lWoHvYe/eladmin/actions/workflows/maven-publish.yml/badge.svg)](https://github.com/lWoHvYe/eladmin/actions/workflows/maven-publish.yml)
</div>

本项目在eladmin项目的基础上，进行了部分扩展及尝试，在此表示感谢。

---
本分支将停留在17版本。另将在`dev_3.x`分支，尝试后续版本的JDK（主要是Virtual Thread），待21中VT GA后，会将其merge到main branch.

启动类 [AppRun.java](unicorn-starter/src/main/java/com/lwohvye/AppRun.java)
和配置文件 [resources](unicorn-starter/src/main/resources)详见 [unicorn-starter](unicorn-starter) 模块。[启停脚本](script)。 
You can find the minimum to run in [Valentine's Day](valentine-starter).
~~注：模块化当前只支持研发模式，要打包部署需要将[module-info.java](unicorn-starter/src/main/java/module-info.java)
删除，以非module化运行，模块化打包部署暂未找到支持外置配置及依赖的方式~~
，模块化package已完成，只是无法从Jar中剔除配置，外置配置也是支持的，根据加载规则，外置的配置项会覆盖内置的

**Java16**之后，默认强封装JDK内部类，详见[JEP 396](https://openjdk.java.net/jeps/396)
[JEP 403](https://openjdk.java.net/jeps/403)，需在启动时添加相关参数开启包访问。较简单的是添加
``--add-opens java.base/java.lang=ALL-UNNAMED`` ，也可根据需要缩小范围（在Java 9引入的JPMS/Jigsaw）。
详见：[Java 16](document/jdk/Java-16.md) [Java 17](document/jdk/Java-17.md)

后台运行jar（开启远程调试端口5005）。2>&1 表示在同一个文件中同时捕获 System.err和
System.out（有一个箭头的表示以覆盖的方式重定向，而有两个箭头的表示以追加的方式重定向。如果需要将标准输出以及标准错误输出同时重定向到一个文件，需要将某个输出转换为另一个输出，例如
2>&1
表示将标准错误输出转换为标准输出）。

```shell
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar unicorn-starter-3.0.0.jar >nohup.out 2>&1 &
```

若外置依赖启动参数需添加，``-Dloader.path=lib``引入依赖。外置依赖可以大大减少jar包的体积。方便后续更新部署

```shell
#2.x版本启动示例
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -Dloader.path=lib -jar eladmin-starter-2.6.18.jar >nohup.out 2>&1 &
```

```shell
#3.x版本开始，因为已完成JPMS改造，可移除启动参数中 --add-opens 部分
nohup java -XX:+UseZGC -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -Dloader.path=lib -jar unicorn-starter-3.1.0.jar >nohup.out 2>&1 &
```

| key                | 目的                                                                                                                                                           |
|--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| loader.path        | lib包加载路径                                                                                                                                                     |
| loader.home        | 用于解析loader.path中的相对路径。 例如，给定loader.path = lib，则$ {loader.home} / lib是类路径位置（以及该目录中的所有jar文件）。 此属性还用于查找loader.properties文件，如以下示例/ opt / app所示。它默认为$ {user.dir}。 |
| loader.args        | main方法的默认参数（以空格分隔）                                                                                                                                           |
| loader.main        | 要启动的主类的名称（例如com.app.Application）                                                                                                                             |
| loader.config.name | 属性文件的路径（例如，classpath：loader.properties）。 默认为loader.properties。                                                                                               |
| loader.system      | 布尔值标志，指示应将所有属性添加到系统属性。 默认为false。                                                                                                                             |

参考：[executable-jar.launching](https://docs.spring.io/spring-boot/docs/current/reference/html/executable-jar.html#executable-jar.launching)

---

#### Maven引用方式 🎵

最新版本为: [![Maven Central](https://img.shields.io/maven-central/v/com.lwohvye/unicorn.svg?logo=github&style=flat)](https://mvnrepository.com/artifact/com.lwohvye/unicorn)

**可根据需要选择版本**

```xml
<!--2.6.18版本为springfox + 未进行动态权限改造-->
<!-- https://mvnrepository.com/artifact/com.lwohvye/eladmin -->
<dependency>
    <groupId>com.lwohvye</groupId>
    <artifactId>eladmin</artifactId>
    <version>2.6.18</version>
    <type>pom</type>
</dependency>
```

```xml
<!--3.x系列版本为springdoc + 动态权限改造 + JPMS部分改造-->
<dependency>
    <groupId>com.lwohvye</groupId>
    <artifactId>unicorn</artifactId>
    <version>3.2.0</version>
    <type>pom</type>
</dependency>

```

---

#### 项目简介

一个基于最新的Java 17版本、 Spring Boot 3.0、 Jpa、 Spring Security、
Redis、ShardingSphere、RabbitMQ、Vue的前后端分离的系统。在各模块基本解耦之后，可根据需要只引入部分模块实现相关职能。

#### 项目源码

|            | 后端源码                               | 前端源码                                   |
|------------|------------------------------------|----------------------------------------|
| 原项目-github | https://github.com/elunez/eladmin  | https://github.com/elunez/eladmin-web  |
| 原项目-码云     | https://gitee.com/elunez/eladmin   | https://gitee.com/elunez/eladmin-web   |
| github     | https://github.com/lWoHvYe/eladmin | https://github.com/lWoHvYe/eladmin-web |

#### 主要特性

- 使用最新技术栈，社区资源丰富，基于Java 17、Spring Boot 3.0。
- 基于注解的动态查询（Specification），可根据需要扩充查询注解。
- 支持数据字典，可方便地对一些状态进行管理
- 高效率开发，代码生成器可一键生成前后端代码
- 支持接口级别的功能权限与数据权限，可自定义操作
- 自定义权限注解与匿名接口注解，可快速对接口拦截与放行
- 对一些常用前端组件封装：表格数据请求、数据字典等
- 前后端统一异常拦截处理，统一输出异常，避免繁琐的判断
- 提供服务器性能监控功能
- 支持运维管理，可方便地对远程服务器的应用进行部署与管理
- 使用ShardingSphere实现多数据源和读写分离。该方式针对Mysql数据库。对系统侵入性小。（只需引入依赖，并在yaml中配置数据源信息即可）。
- 整合Redisson拓展Redis的功能，读写分离
- 整合消息队列RabbitMQ，实现消息通知、延迟消息，服务解耦。
- 各模块独立，基本可插拔：若只需查询注解类基础功能，只需引入core模块即可，权限、日志、3rd Tools模块可插拔可独立部署，
  除了传统To B业务，还可用于To C业务

#### 系统功能

- 用户管理：提供用户的相关配置，新增用户后，默认密码为123456
- 角色管理：对权限与菜单进行分配，可根据部门设置角色的数据权限
- 菜单管理：已实现菜单动态路由，后端可配置化，支持多级菜单
- 部门管理：可配置系统组织架构，树形表格展示
- 岗位管理：配置各个部门的职位
- 字典管理：可维护常用一些固定的数据，如：状态，性别等
- 系统日志：记录用户操作日志与异常日志，方便开发人员定位排错
- SQL监控：采用druid 监控数据库访问性能，默认用户名admin，密码admin
- 定时任务：整合Quartz做定时任务，加入任务日志，任务运行情况一目了然
- 代码生成：高灵活度生成前后端代码，减少大量重复的工作任务（逆向有很多方案，这种基于template的有一定的灵活性）
- 邮件工具：配合富文本，发送html格式的邮件
- 服务监控：监控服务器的负载情况

#### 项目结构

项目采用按功能分模块的开发方式，结构如下

- `unicorn-core` 系统的核心模块，各种工具类，公共配置存在该模块

- `unicorn-reactive` core模块默认基于MVC，因为Gateway是基于WebFlux的，以此进行适配

- `unicorn-sys-api` Sys Module基础实体及API，方便服务拆分

- `unicorn-security` 系统权限模块，包含权限配置管理等。

- `unicorn-logging` 系统的日志模块，其他模块如果需要记录日志需要引入该模块

- `unicorn-tp-tools` 第三方工具模块，包含：邮件、S3，可视情况引入

- `unicorn-code-gen` 系统的代码生成模块，代码生成的模板在 system 模块中。这部分待优化，亦非必须模块

- `unicorn-starter` 启动类，项目入口，包含模块及组件配置（DB读写分离 + Cache读写分离），枚举类动态扩展的简单demo

- `valentine-search` 通过mongodb进行最基础的检索，整合elasticsearch，SPI相关demo

- `valentine-starter` 启动配置示例，最小环境依赖启动

#### 详细结构

```
- unicorn-core 公共模块
    - annotation 为系统自定义注解
    - aspect 自定义注解的切面
    - base 提供了Entity、Service、DTO基类和mapstruct的通用mapper
    - config 自定义权限实现、redis配置、openApi配置、Rsa配置等
        - security 权限控制，为swarm化，提供全局关闭Security功能
    - exception 项目统一异常的处理
    - utils 系统通用工具类
- unicorn-sys-api 基础实体及DTO
    - annotation 为模块自定义注解
    - modules 基础实体及接口定义
    - utils 通用工具类扩展
- unicorn-security 系统核心模块
	- common 配置跨域、静态资源、数据权限、DB Insert主键、实体表映射、系统完成入口
	    - init 容器启动后的钩子call back
	    - orm jpa-entity的部分配置，eg: Table Mapping
	    - thread 线程池相关
	    - web corsFilter configurer and so on
	- modules 系统相关模块(登录授权、消息队列、系统监控、定时任务、运维管理等)
	    - quartz 定时任务
	    - rabbitmq 消息队列相关
	    - security 权限控制
	    - system 用户-权限管理
- unicorn-starter 系统启动入口。相关示例
- unicorn-logging 系统日志模块
- unicorn-tp-tools 系统第三方工具模块
- unicorn-code-gen 系统代码生成模块
```

#### 运行环境

- Java 17 基础运行环境
- Mysql 5.7/8.0 数据库 读写分离/单数据源-通过配置数据源的方式切换
- Redis 6.0 缓存
- RabbitMQ 发布-订阅（解耦、异步）
- ELK 日志系统，若不需要可调整logback-spring.xml中的配置
- 可基于docker [一键搭建](document/docker/docker-compose-env.yml)。当然目录还是要自己建的，另外RabbitMQ记得装延迟插件，ES记得装IK
- 若想搭建k8s版，可[参照](https://github.com/lWoHvYe/mall-swarm/tree/main/document/kubernetes/env) ,这个是基于NFS挂载的。

#### 特别鸣谢

- 感谢 [JetBrains](https://www.jetbrains.com/) 提供的非商业开源软件开发授权

- 感谢 [PanJiaChen](https://github.com/PanJiaChen/vue-element-admin) 大佬提供的前端模板

- 感谢 [Moxun](https://github.com/moxun1639) 大佬提供的前端 Curd 通用组件

- 感谢 [zhy6599](https://gitee.com/zhy6599) 大佬提供的后端运维管理相关功能

- 感谢 [j.yao.SUSE](https://github.com/everhopingandwaiting) 大佬提供的匿名接口与Redis限流等功能

- 感谢 [d15801543974](https://github.com/d15801543974) 大佬提供的基于注解的通用查询方式

- 感谢 [elunez](https://github.com/elunez) 大佬提供的eladmin项目

---

#### Feature list

- dev_3.0 Springdoc相关。Web侧跟进（无限delay）
- ASM字节码增强
- 授权(Authorization)模块-颁发及刷新Token （accessToken & refreshToken）Jwt Token 都是成对出现的，一个为平常请求携带的
  accessToken， 另一个只作为刷新 accessToken 用的 refreshToken
- dev_3.0 JPMS改造（3.0版本有做部分尝试，当前在IDEA中可开发调试，但模块化打包部署尚未以Named Module的方式运行，
  推测是Spring Boot的 ClassLoader下全是Auto-Module）
- swarm化，可以参考[why-swarm (施工中)](https://github.com/WHY-lWoHvYe/why-swarm)

#### TODO

- 使用ShardingSphere 读写分离，负载均衡算法可以选择ROUND_ROBIN 或 TRANSACTION_ROUND_ROBIN,前者事务内全部走primary，
  理论上这种方式更好(复制延迟、同步时机,update中的select最好走primary)，
  但为了解决懒加载no-session的问题， 很多复杂查询都加了事务注解，这样如果用第一种，主库的压力会比较大，而第二种会有上面提到的问题,
  后续再看看吧(寻求其他no-session的解决方案,第二种配合强制路由).
  补充：同一事务内，在Update之后Select，似乎走的是Primary，如果这样的话，用第二种似乎就可以了
