**Java16**之后，默认强封装JDK内部类，详见[JEP 396](https://openjdk.java.net/jeps/396) [JEP 403](https://openjdk.java.net/jeps/403) ，需在启动时添加相关参数。较简单的是添加
``--add-opens java.base/java.lang=ALL-UNNAMED`` ，也可根据需要缩小范围

后台运行jar（开启远程调试端口5005）

```shell
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar eladmin-starter-2.6.18.jar >nohup.out 2>&1 &
```

#### Run

```
    针对异常
    java.lang.reflect.InaccessibleObjectException: Unable to make field private final java.lang.String java.lang.NamedPackage.name accessible: module java.base does not "opens java.lang" to unnamed module @7bfcd12c
    需要在jvm 中添加启动参数
     --add-opens java.base/java.lang=ALL-UNNAMED 
     注：在Java 9 中引入了Java平台模块系统（JPMS），这里的ALL-UNNAMED表示所有未命名模块。整体意思为将java.base/java.lang对所有未命名模块开放
     根据java.base模块的module-info.java文件，其已将主要的所有的类exports。在运行时需要反射获取java.base/java.lang下的类，所以要添加opens，这里应该是未把其子包open的
    如果反射代码在命名模块中，则ALL-UNNAMED可以用其名称替换。
    如果要添加的标志太多，则可以考虑使用封装终止开关 --permit-illegal-access。它将允许类路径上的所有代码反射所有已命名的模块
    针对Java 16下，无法反射java.base包下的内容的情况，在mvn的运行环境中添加如下配置
        --illegal-access
        该参数有四个可选值：
        permit：Java 9开始默认值，允许通过反射访问，因此会提示像上面一样的警告，这个是首次非法访问警告，后续不警告
        warn：每次非法访问都会警告
        debug：在warn的基础上加入了类似e.printStackTrace()的功能
        deny：Java 16开始默认值。禁止所有的非法访问除了使用特别的命令行参数排除的模块，比如使用--add-opens排除某些模块使其能够通过非法反射访问
        Java 17移除了 --illegal-access，需要在mvn中使用--add-opens配置
        
```

#### Maven

```xml
            <!-- 解决 Java-16下 lombok报错：
            Fatal error compiling: java.lang.IllegalAccessError: class lombok.javac.apt.LombokProcessor (in unnamed module @0x486bcaa) cannot access class com.sun.tools.javac.processing.JavacProcessingEnvironment (in module jdk.compiler) because module jdk.compiler does not export com.sun.tools.javac.processing to unnamed module @0x486bcaa
            在最新的JDK-16构建中看到的异常是由于jep396：默认情况下强封装JDK内部构件。Lombok正在使用反射访问一个内部jdkapi，在以前的Java版本中，这会导致警告消息，现在则会导致硬错误。
            一般来说，在运行java时，通过在运行java时传递--add-opens=<module>/<package>=<accessing module>指令作为VM参数，可以显式地打开内部JDK包进行反射。在这种情况下，需要将这些指令传递给调用javac时运行的java进程。这可以通过在传递给javac的选项前面加上-J来完成，后者将把它传递给底层JVM。
            其中需要的选项在配置中使用<compilerArgs>元素传递。
            注意，我在选项前面添加了-J，以便将它们传递给运行javac的JVM，而不是javac选项。
            在问题中列出的--add-opens指令之外，还有一个额外的：
                -J--add-opens=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED
            也是需要的。
            <fork>true</fork>也是必需的，否则-J选项将被忽略（从mvn clean install -X的输出判断）。查看Maven文档，在使用<compilerArgs>时，似乎需要随时将fork设置为true：
            参考：https://maven.apache.org/plugins/maven-compiler-plugin/compile-mojo.html#compilerArgs
            <compilerArgs>设置在fork设置为true时传递给编译器的参数。
            -->
<plugin>
    <!-- maven-compiler-plugin应该是默认使用的。所以可以不显示的配置。这里保留主要是为了记录 --add-opens=... 配置的方式 -->
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-compiler-plugin</artifactId>
    <configuration>
        <source>17</source>
        <target>17</target>
        <!--<release>16</release>-->
        <!--<fork>true</fork>-->
        <!-- The flowing is for lombok version before 1.18.20 -->
        <compilerArgs>
            <arg>--enable-preview</arg>
            <arg>-Xlint:all</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.comp=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.main=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.model=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED</arg>
        </compilerArgs>
        <!--for unmappable characters in classes-->
        <!-- <encoding>UTF-8</encoding>-->
        <showDeprecation>true</showDeprecation>
        <showWarnings>true</showWarnings>
        <!--for lombok annotations to resolve-->
        <!--contradictory to maven, intelliJ fails with this-->
        <annotationProcessorPaths>
            <!--4lombok-->
            <path>
                <groupId>org.projectlombok</groupId>
                <artifactId>lombok</artifactId>
                <!-- Support for Java 16
                https://github.com/rzwitserloot/lombok/issues/2681#
                -->
                <version>${lombok.version}</version>
            </path>
            <!--4mapstruct-->
            <path>
                <groupId>org.mapstruct</groupId>
                <artifactId>mapstruct-processor</artifactId>
                <version>${mapstruct.version}</version>
            </path>
        </annotationProcessorPaths>
        <!--https://docs.spring.io/spring-boot/docs/2.4.4/reference/html/appendix-configuration-metadata.html#configuration-metadata-annotation-processor-->
        <!--<proc>none</proc>-->
    </configuration>
</plugin>
```

#### lombok

在Java 16+的Runtime。需要lombok使用1.18.20+的版本

[Support for JDK16](https://github.com/rzwitserloot/lombok/issues/2681#)

#### Java平台模块系统（JPMS）、Jigsaw 项目

这个确切的讲，是在Java 9引入的

```
JPMS对包可见性细化为：public to everyone、public but only to friend modules、public only within a module、protected、package、private
模块系统的首要目的是为了封装。然后在有些时候，我们必须要打破封装来处理遗留代码或是运行测试。我们可以下面几个命令行参数来打破封装。
需注意的是，包是没有层级关系的，也就是说java.base/java.lang配置只针对该包下的类，是不包括子包的，也就是针对java.lang.reflect要单独配置
--add-reads module=target-module(,target-module)*：更新源模块来读取目标模块。目标模块可以是ALL-UNNAMED来读取所有未命名模块。
--add-exports module/package=target-module(,target-module)*：更新源模块来导出包到目标模块。这会添加一个从源模块来目标模块的受限导出。目标模块可以是ALL-UNNAMED来导出到所有未命名模块。
--add-opens module/package=target-module(,target-module)*：更新源模块来开放包到目标模块。这回添加一个从源模块到目标模块的受限开放。
--patch-module module=file(;file)*：使用JAR文件或目录中的类和资源文件来覆盖或增加一个模块的内容。在需要临时修改一个模块的内容以方便测试时，--patch-module非常实用。
```

Java 为 module-info.java 设计了专用的语法

```
语法解读：(这里同样是不包括子包)

[open] module <module>: 声明一个模块，模块名称应全局唯一，不可重复。加上 open 关键词表示模块内的所有包都允许通过 Java 反射访问，模块声明体内不再允许使用 opens 语句。

requires [transitive | static] <module>: 声明模块依赖，一次只能声明一个依赖，如果依赖多个模块，需要多次声明。加上 transitive 关键词表示传递依赖，比如模块 A 依赖模块 B，模块 B 传递依赖模块 C，那么模块 A 就会自动依赖模块 C，类似于 Maven。加上 static 关键词表示编译时必需，运行时可选，类似于 Maven 的 <scope>runtime</scope>。

exports <package> [to <module1>[, <module2>...]]: 导出模块内的包（允许直接 import 使用），一次导出一个包，如果需要导出多个包，需要多次声明。如果需要定向导出，可以使用 to关键词，后面加上模块列表（逗号分隔）。

opens <package> [to <module>[, <module2>...]]: 开放模块内的包（允许通过 Java 反射访问），一次开放一个包，如果需要开放多个包，需要多次声明。如果需要定向开放，可以使用 to关键词，后面加上模块列表（逗号分隔）。

provides <interface | abstract class> with <class1>[, <class2> ...]: 声明模块提供的 Java SPI 服务，一次可以声明多个服务实现类（逗号分隔）。

uses <interface | abstract class>: 声明模块依赖的 Java SPI 服务，加上之后模块内的代码就可以通过 ServiceLoader.load(Class) 一次性加载所声明的 SPI 服务的所有实现类。
```

在实际使用中，

- 需要容器创建的bean，需要exports to spring.beans, 有的还需要spring.content，暂不清楚；
- 包含注入的属性的bean，需要opens to spring.core。~~opens包含了exports~~，当需要显示import时，只open是不行的，还是要export
- controller需要exports to spring.web。
- Aspect相关的需要 exports to aop
- 还有那些需要反射的场景

那些在resources中的配置等内容，也需要将相关目录open，可参考classes下的结构，否则无法被外界访问，最容易漏掉的就是config中的spring boot配置。

应该是有相关插件，用于生成module-info.java文件的。

- [Project Jigsaw](http://openjdk.java.net/projects/jigsaw/quick-start)
- [warn: requires transitive directive for an automatic module](https://stackoverflow.com/questions/49600947/how-to-suppress-the-requires-transitive-directive-for-an-automatic-module-warn)
- [jmod-example](https://github.com/khmarbaise/jdk9-jlink-jmod-example)

基于下面这俩链接，基本说明了在下一个大版本，将对JPMS进行支持

- [Spring Boot Java 9+ modularity](https://github.com/spring-projects/spring-boot/issues/13799)
- [Declare Spring modules with JDK 9 module metadata [SPR-13501]](https://github.com/spring-projects/spring-framework/issues/18079)

```shell
# 将无法模块化的放到clib中，可模块化的放到mlib，将未升级模块的依赖放入mlib中，会自动模块化，称为自动模块
# 当前待解决：模块 lwohvye.eladmin.starter 不具有 ModuleMainClass 属性，请使用 -m <模块>/<主类>。针对Spring Boot项目，应该有某个地方不一样
java -p mlib -Dloader.path=clib -m lwohvye.eladmin.starter
```

- ~~在Idea的 Run/Debug Configurations中的VM options中，部分可能需要调一下~~
- 直接启动报异常，是因为部分依赖无法module化（无法得到 module description，在编译时有相关警告 can't extract module name from xxx.jar: Provider class xxx not int
  module），[具体原因](https://stackoverflow.com/questions/54682417/unable-to-derive-module-descriptor-provider-class-x-not-in-module)

```
Error occurred during initialization of boot layer
java.lang.module.FindException: Unable to derive module descriptor for xxx.jar
Caused by: java.lang.module.InvalidModuleDescriptorException: Provider class xxx.xxx.xxx not in module
```

- 经简单验证，若无无法module化的依赖，是可以启动成功的
- 另，若删除主启动类的module-info.java，以未命名模块的方式来运行，也是一种方式，这算是对Java 9之前的一个兼容

IDEA中，两种启动方式的启动参数，另通过查看VM参数，module模式中有`--module-path`属性

```shell
# 非module
java -classpath lib com.lwohvye.AppSearchRun
# module
java -classpath lib -m lwohvye.eladmin.starter/com.lwohvye.AppRun
```

[SpringBoot jar包启动原理](https://www.lwohvye.com/2022/03/09/springboot-jar%e5%8c%85%e5%90%af%e5%8a%a8%e7%9a%84%e5%8e%9f%e7%90%86/)
