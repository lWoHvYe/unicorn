<h1 style="text-align: center">EL-ADMIN 后台管理系统</h1>
<div style="text-align: center">

[![AUR](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg)](https://github.com/lWoHvYe/eladmin/blob/main/LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/lWoHvYe/eladmin.svg?style=social&label=Stars)](https://github.com/lWoHvYe/eladmin)
[![GitHub forks](https://img.shields.io/github/forks/lWoHvYe/eladmin.svg?style=social&label=Fork)](https://github.com/lWoHvYe/eladmin)

</div>

本项目在原eladmin项目的基础上，进行了部分扩展及尝试，在此表示感谢。

启动类及配置文件，参照 eladmin-starter模块

**Java16**之后，默认强封装JDK内部类，详见[JEP 396](https://openjdk.java.net/jeps/396) [JEP 403](https://openjdk.java.net/jeps/403) ，需在启动时添加相关参数。较简单的是添加 
``--add-opens java.base/java.lang=ALL-UNNAMED`` ，也可根据需要缩小范围

后台运行jar（开启远程调试端口5005）
```shell
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar eladmin-starter-2.6.17.jar >nohup.out 2>&1 &
```

若外置依赖启动参数需添加，``-Dloader.path=lib``引入依赖。外置依赖可以大大减少jar包的体积。方便后续更新部署

```shell
#启动示例
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -Dloader.path=lib -jar eladmin-starter-2.6.17.jar >nohup.out 2>&1 &
```

| key                | 目的                                                         |
| ------------------ | ------------------------------------------------------------ |
| loader.path        | lib包加载路径                                                |
| loader.home        | 用于解析loader.path中的相对路径。 例如，给定loader.path = lib，则$ {loader.home} / lib是类路径位置（以及该目录中的所有jar文件）。 此属性还用于查找loader.properties文件，如以下示例/ opt / app所示。它默认为$ {user.dir}。 |
| loader.args        | main方法的默认参数（以空格分隔）                             |
| loader.main        | 要启动的主类的名称（例如com.app.Application）                |
| loader.config.name | 属性文件的路径（例如，classpath：loader.properties）。 默认为loader.properties。 |
| loader.system      | 布尔值标志，指示应将所有属性添加到系统属性。 默认为false。   |

参考：https://docs.spring.io/spring-boot/docs/current/reference/html/executable-jar.html#executable-jar.launching

```
在Spring Boot 2.5版本存在报错(在2.5.1已修复)：（使用Idea时正常，jar运行时报错）
java.lang.IllegalStateException: No subdirectories found for mandatory directory location 'file:./config/*/'
解决方式为添加启动参数    --spring.config.location=optional:classpath:/,optional:classpath:/config/,optional:file:./,optional:file:./config/
参考：https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#features.external-config.files
```


在Spring Boot 2.6.0版本，启动报错PatternsRequestCondition.getPatterns()空指针，原因详见[issues](https://github.com/springfox/springfox/issues/3462) 。该版本Spring boot的 [ Release-Notes ](https://github.com/spring-projects/spring-boot/wiki/Spring-Boot-2.6-Release-Notes)


**Java 17**，发布中央仓库，需要在maven的vm中配置。若不需要deploy，无需添加
```
--add-opens java.base/java.lang=ALL-UNNAMED
--add-opens java.base/java.lang.reflect=ALL-UNNAMED
--add-opens java.base/java.util=ALL-UNNAMED
--add-opens java.base/java.text=ALL-UNNAMED
--add-opens java.desktop/java.awt.font=ALL-UNNAMED
```

#### Maven引用方式 🎵 
最新版本为: [![Maven Central](https://img.shields.io/maven-central/v/com.lwohvye/eladmin.svg?logo=github&style=flat)](https://mvnrepository.com/artifact/com.lwohvye/eladmin)

```xml
<!-- https://mvnrepository.com/artifact/com.lwohvye/eladmin -->
<dependency>
    <groupId>com.lwohvye</groupId>
    <artifactId>eladmin</artifactId>
    <version>2.6.16</version>
    <type>pom</type>
</dependency>

```

#### 项目简介

一个基于 Spring Boot 2.5.6 、 Spring Boot Jpa、 JWT、Spring Security、Redis、ShardingSphere、RabbitMQ、Vue的前后端分离的后台管理系统

**开发文档：**  [https://el-admin.vip](https://el-admin.vip)

**体验地址：**  [https://el-admin.xin](https://el-admin.xin)

**账号密码：** `admin / 123456`

#### 项目源码

|     |   后端源码  |   前端源码  |
|---  |--- | --- |
|  原项目-github   |  https://github.com/elunez/eladmin   |  https://github.com/elunez/eladmin-web   |
|  原项目-码云   |  https://gitee.com/elunez/eladmin   |  https://gitee.com/elunez/eladmin-web   |
|  github   |   https://github.com/lWoHvYe/eladmin |    https://github.com/lWoHvYe/eladmin-web |

#### 主要特性

- 使用最新技术栈，社区资源丰富。
- 高效率开发，代码生成器可一键生成前后端代码
- 支持数据字典，可方便地对一些状态进行管理
- 支持接口限流，避免恶意请求导致服务层压力过大
- 支持接口级别的功能权限与数据权限，可自定义操作
- 自定义权限注解与匿名接口注解，可快速对接口拦截与放行
- 对一些常用地前端组件封装：表格数据请求、数据字典等
- 前后端统一异常拦截处理，统一输出异常，避免繁琐的判断
- 支持在线用户管理与服务器性能监控，支持限制单用户登录
- 支持运维管理，可方便地对远程服务器的应用进行部署与管理
- 使用ShardingSphere实现多数据源和读写分离（Sharding-JDBC）。该方式针对Mysql数据库。对系统侵入性小。（只需引入依赖，并在yaml中配置数据源信息即可）。若需要分库分表，可参考[jpa-分库分表](https://github.com/lWoHvYe/spring-boot-jpa-cascade)
- Redis多数据源支持（已改回单节点并整合Redisson拓展Redis的功能），集群中，可将Token存入特定的Redis中，其他缓存到各自的Redis。即实现了集群间的Session共享，有减少集群各节点间的影响
- 整合消息队列RabbitMQ，实现消息通知、延迟消息。
- 基于最新的Java-17。

#### 系统功能

- 用户管理：提供用户的相关配置，新增用户后，默认密码为123456
- 角色管理：对权限与菜单进行分配，可根据部门设置角色的数据权限
- 菜单管理：已实现菜单动态路由，后端可配置化，支持多级菜单
- 部门管理：可配置系统组织架构，树形表格展示
- 岗位管理：配置各个部门的职位
- 字典管理：可维护常用一些固定的数据，如：状态，性别等
- 系统日志：记录用户操作日志与异常日志，方便开发人员定位排错
- SQL监控：采用druid 监控数据库访问性能，默认用户名admin，密码123456
- 定时任务：整合Quartz做定时任务，加入任务日志，任务运行情况一目了然
- 代码生成：高灵活度生成前后端代码，减少大量重复的工作任务
- 邮件工具：配合富文本，发送html格式的邮件
- 七牛云存储：可同步七牛云存储的数据到系统，无需登录七牛云直接操作云数据
- 阿里云OSS：可实现基础的上传及下载功能
- 支付宝支付：整合了支付宝支付并且提供了测试账号，可自行测试
- 服务监控：监控服务器的负载情况
- 运维管理：一键部署你的应用

#### 项目结构

项目采用按功能分模块的开发方式，结构如下

- `eladmin-common` 为系统的公共模块，各种工具类，公共配置存在该模块

- `eladmin-api` 基础实体及接口模块，方便后续服务拆分

- `eladmin-system` 为系统核心模块，包含管理侧权限配置等。包含api模块service层的具体实现

- `eladmin-logging` 为系统的日志模块，其他模块如果需要记录日志需要引入该模块

- `eladmin-tools` 为第三方工具模块，包含：图床、邮件、云存储、本地存储、支付宝

- `eladmin-generator` 为系统的代码生成模块，代码生成的模板在 system 模块中

- `eladmin-starter` 启动类,项目入口，包含模块及组建配置

- `eladmin-search` 通过mongodb进行最基础的检索，整合elasticsearch，SPI相关demo

#### 详细结构

```
- eladmin-common 公共模块
    - annotation 为系统自定义注解
    - aspect 自定义注解的切面
    - base 提供了Entity、DTO基类和mapstruct的通用mapper
    - config 自定义权限实现、redis配置、swagger配置、Rsa配置等
    - exception 项目统一异常的处理
    - utils 系统通用工具类
- eladmin-api 基础实体及接口模块
    - annotation 为模块自定义注解
    - modules 基础实体及接口定义
    - utils 通用工具类扩展
- eladmin-system 系统核心模块
	- config 配置跨域与静态资源，与数据权限
	    - thread 线程池相关
	    - rabbitmq 消息队列相关
	- modules 系统相关模块(登录授权、消息队列、系统监控、定时任务、运维管理等)
- eladmin-starter 系统启动入口。相关示例
- eladmin-logging 系统日志模块
- eladmin-tools 系统第三方工具模块
- eladmin-generator 系统代码生成模块
```

#### 运行环境

- Java 17 基础运行环境
- Mysql 5.7/8.0 数据库 读写分离/单数据源-通过配置数据源的方式切换
- Redis 6.0 缓存
- RabbitMQ 鉴权结果记录、用户多次验证失败锁定一段时间后自动解锁
- ELK 日志系统，若不需要可调整logback-spring.xml中的配置

#### 特别鸣谢

- 感谢 [JetBrains](https://www.jetbrains.com/) 提供的非商业开源软件开发授权

- 感谢 [七牛云](https://www.qiniu.com/) 提供的免费云存储与CDN加速支持

- 感谢 [PanJiaChen](https://github.com/PanJiaChen/vue-element-admin) 大佬提供的前端模板

- 感谢 [Moxun](https://github.com/moxun1639) 大佬提供的前端 Curd 通用组件

- 感谢 [zhy6599](https://gitee.com/zhy6599) 大佬提供的后端运维管理相关功能

- 感谢 [j.yao.SUSE](https://github.com/everhopingandwaiting) 大佬提供的匿名接口与Redis限流等功能

- 感谢 [d15801543974](https://github.com/d15801543974) 大佬提供的基于注解的通用查询方式

- 感谢 [elunez](https://github.com/elunez) 大佬提供的eladmin项目

#### 项目捐赠

项目的发展离不开你的支持，请作者喝杯咖啡吧☕  [Donate](https://el-admin.vip/donation/)

#### 反馈交流

- QQ交流群：一群：<strike>891137268</strike> 已满、二群：947578238

#### 启动类示例

```java

/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye;

import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.utils.SpringContextHolder;
import io.swagger.annotations.Api;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.bind.annotation.RestController;

/**
 * 开启审计功能 -> @EnableJpaAuditing
 *
 * @author Zheng Jie
 * @date 2018/11/15 9:20:19
 */
@EnableAsync
@RestController
@Api(hidden = true)
@SpringBootApplication
@EnableTransactionManagement
@EnableJpaAuditing(auditorAwareRef = "auditorAware")
@EnableRetry //开启重试机制
//开启 @ConfigurationProperties 注解
@EnableConfigurationProperties
public class AppRun {

    public static void main(String[] args) {
        SpringApplication.run(AppRun.class, args);
    }

    @Bean
    public SpringContextHolder springContextHolder() {
        return new SpringContextHolder();
    }

    /**
     * 访问首页提示
     *
     * @return /
     */
    @AnonymousGetMapping("/")
    public String index() {
        return "Backend service started successfully";
    }
}
```

#### MapStruct介绍

| Option                                           | Purpose                                                      | Default   |
| :----------------------------------------------- | :----------------------------------------------------------- | :-------- |
| `mapstruct. suppressGeneratorTimestamp`          | If set to `true`, the creation of a time stamp in the `@Generated` annotation in the generated mapper classes is suppressed. | `false`   |
| `mapstruct.verbose`                              | If set to `true`, MapStruct in which MapStruct logs its major decisions. Note, at the moment of writing in Maven, also `showWarnings` needs to be added due to a problem in the maven-compiler-plugin configuration. | `false`   |
| `mapstruct. suppressGeneratorVersionInfoComment` | If set to `true`, the creation of the `comment` attribute in the `@Generated` annotation in the generated mapper classes is suppressed. The comment contains information about the version of MapStruct and about the compiler used for the annotation processing. | `false`   |
| `mapstruct.defaultComponentModel`                | The name of the component model (see [Retrieving a mapper](https://mapstruct.org/documentation/stable/reference/html/#retrieving-mapper)) based on which mappers should be generated.<br>Supported values are:<br>`default`: the mapper uses no component model, instances are typically retrieved via `Mappers#getMapper(Class)`<br>`cdi`: the generated mapper is an application-scoped CDI bean and can be retrieved via `@Inject`<br>`spring`: the generated mapper is a singleton-scoped Spring bean and can be retrieved via `@Autowired`<br>`jsr330`: the generated mapper is annotated with {@code @Named} and can be retrieved via `@Inject`, e.g. using Spring <br> If a component model is given for a specific mapper via `@Mapper#componentModel()`, the value from the annotation takes precedence. | `default` |
| `mapstruct.defaultInjectionStrategy`             | The type of the injection in mapper via parameter `uses`. This is only used on annotated based component models such as CDI, Spring and JSR 330.<br>Supported values are:<br>`field`: dependencies will be injected in fields<br>`constructor`: will be generated constructor. Dependencies will be injected via constructor.<br>When CDI `componentModel` a default constructor will also be generated. If a injection strategy is given for a specific mapper via `@Mapper#injectionStrategy()`, the value from the annotation takes precedence over the option. | `field`   |
| `mapstruct.unmappedTargetPolicy`                 | The default reporting policy to be applied in case an attribute of the target object of a mapping method is not populated with a source value.<br>Supported values are:<br>`ERROR`: any unmapped target property will cause the mapping code generation to fail<br>`WARN`: any unmapped target property will cause a warning at build time<br>`IGNORE`: unmapped target properties are ignored<br>If a policy is given for a specific mapper via `@Mapper#unmappedTargetPolicy()`, the value from the annotation takes precedence. | `WARN`    |

MapStruct 提供的重要注解 :

@Mapper : 标记这个接口作为一个映射接口，并且是编译时 MapStruct 处理器的入口

@Mapping : 解决源对象和目标对象中，属性名字不同的情况

部署脚本

```shell
mv -f /opt/upload/eladmin-system-2.6.17.jar /opt/app
cd /opt/app
nohup /usr/java/jdk-14/bin/java -jar eladmin-system-2.6.17.jar >nohup.out 2>&1 &
```

启动脚本

```shell
#!/bin/bash
cd /opt/app
echo "执行...."
nohup /usr/java/jdk-17/bin/java -jar eladmin-system-2.6.17.jar >nohup.out 2>&1 &
echo "启动成功"
```

停止脚本

```shell
#!/bin/bash
echo "stop SpringBoot BAppApiServerApplication"
# shellcheck disable=SC2009
pid=$(ps -ef | grep eladmin-system-2.6.17.jar | grep -v grep | awk '{print $2}')
echo "旧应用进程id：$pid"
if [ -n "$pid" ]
then
# 通过使用-15 而不是-9 来停止线程
kill -15 "$pid"
fi
```

#### TODO
- 整合Redisson（当前无法配置过期通知，待解决）
- JSON相关调整，使用Jackson替换Fastjson（主体剩余redis序列化/反序列化部分）
